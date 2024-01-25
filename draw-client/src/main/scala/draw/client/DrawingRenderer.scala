package draw.client

import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.{children, textContent}
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.{PathData, SVGOps}
import zio.stream.ZStream
import zio.{Ref, ZIO, ZLayer}

import draw.data.drawcommand.{ContinueScribble, DeleteScribble, DrawCommand, StartScribble}
import draw.data.drawevent.{ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import draw.data.point.Point
import org.scalajs.dom
import draw.data.drawevent.DrawEvent
import zio.lazagna.dom.Alternative
import zio.stream.SubscriptionRef
import zio.lazagna.Consumeable._

trait DrawingRenderer {
  def render(drawing: Drawing): Modifier
}

object DrawingRenderer {
  val live = ZLayer.fromZIO {
    for {
      loading <- SubscriptionRef.make(0)
      currentScribbleId <- Ref.make[Long](0)
      nextScribbleId <- Ref.make[Long](0)
    } yield new DrawingRenderer {
      def render(drawing: Drawing) = {

        val svgBody = g(
          children <~~ drawing.events
            .tap {
              case DrawEvent(_, ScribbleStarted(scribbleId, _, _), _, _, _) =>
                nextScribbleId.update(n => if (n > scribbleId) n else scribbleId + 1)
              case _ =>
                ZIO.unit
            }
            .tap { event => ZIO.when(event.sequenceNr == drawing.initialVersion)(loading.set(1)) }
            .map {
              case DrawEvent(sequenceNr, ScribbleStarted(scribbleId, Some(start), _), _, _, _) =>
                val ID = scribbleId
                val startData = PathData.MoveTo(start.x, start.y)
                val points = d <-- drawing.eventsAfter(sequenceNr)
                  //.aggregateAsyncWithin(ZSink.collectAll, Schedule.fixed(100.milliseconds)) // <-- horribly slow on JS...
                  .chunks
                  .takeUntil(_.exists(_ match { // FIXME Handle update and delete within same small time window
                    case DrawEvent(_, ScribbleDeleted(ID, _), _, _, _) => true
                    case _ => false
                  }))
                  .map(_
                    .collect { case DrawEvent(_, ScribbleContinued(ID, points, _), _, _, _) => points }
                    .flatMap(p => p)
                    .map { pos => PathData.LineTo(pos.x, pos.y) }
                  )
                  .filter(_.size > 0)
                  .mapAccum(Seq[PathData](startData)) { (seq, events) =>
                    //println("Chunk of " + events.size)
                    val res = seq ++ events
                    (res, res)
                  }
                  .map(PathData.render)

                Some(children.Append(
                  g(
                    id := s"scribble${scribbleId}",
                    path(
                      points
                    ),
                    path(
                      cls := "clickTarget",
                      points
                    )
                  )
                ))

              case DrawEvent(_, ScribbleDeleted(id, _), _, _, _) =>
                dom.document.getElementById(s"scribble${id}") match {
                  case null => None
                  case domElmt => Some(children.DeleteDOM(domElmt))
                }

              case _ =>
                None
            }
            .collect { case Some(op) => op }
        )

        val svgLoading = div(
          svg(
            width := 800,
            height := 800,
            rect(
              width := "100%",
              height := "100%",
              fill := "#A88362"
            ),
            text(
              x := 400,
              y := 400,
              textAnchor := "middle",
              textContent := "Loading..."
            )
          )
        )

        val svgMain = div(
          svg(
            width := 800,
            height := 800,
            rect(
              width := "100%",
              height := "100%",
              fill := "#80a080"
            ),
            svgBody,
            SVGOps.coordinateHelper { helper =>
              onMouseDown
                .filter { e => (e.buttons & 1) != 0 }
                .filter(ev => !ev.getModifierState("Alt"))
                .mapZIO(ev => nextScribbleId.get.flatMap(id => currentScribbleId.set(id).as(id)).map { id =>
                  val pos = helper.getClientPoint(ev)
                  DrawCommand(StartScribble(id, Some(Point(pos.x, pos.y))))
                })
                .merge(
                  onMouseDown
                    .merge(onMouseMove)
                    .filter { e => (e.buttons & 1) != 0 }
                    .filter { ev => ev.getModifierState("Alt") }
                    .map(_.target)
                    .collect { case elem: dom.Element => elem }
                    .map(_.parentNode)
                    .collect {
                      case parent:dom.Element if parent.id.startsWith("scribble") =>
                        val id = parent.id.substring("scribble".length).toLong
                        DrawCommand(DeleteScribble(id))
                    }
                )
                .merge(
                  onMouseMove
                    .filter { e => (e.buttons & 1) != 0 }
                    .filter(ev => !ev.getModifierState("Alt"))
                    .mapZIO(ev => currentScribbleId.get.map { id =>
                      val pos = helper.getClientPoint(ev)
                      DrawCommand(ContinueScribble(id, Seq(Point(pos.x, pos.y))))
                    })
                )
                .mapZIO(drawing.perform _)
            }
          )
        )

        Alternative.showOne(Map(
          0 -> svgLoading,
          1 -> svgMain
        ), loading)
      }
    }
  }
}
