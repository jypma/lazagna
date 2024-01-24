package draw.client

import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.children
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Events.given
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.{PathData, SVGOps}
import zio.lazagna.Consumeable.given
import zio.stream.ZStream
import zio.{Ref, ZIO, ZLayer}

import draw.data.drawevent.{ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import org.scalajs.dom
import zio.Hub
import zio.lazagna.dom.Alternative
import draw.data.drawcommand.DrawCommand
import draw.data.drawcommand.StartScribble
import draw.data.point.Point
import draw.data.drawcommand.DeleteScribble
import draw.data.drawcommand.ContinueScribble

trait DrawingRenderer {
  def render(drawing: Drawing): Modifier
}

object DrawingRenderer {
  val live = ZLayer.fromZIO {
    for {
      visibleHub <- Hub.bounded[Int](1)
      currentScribbleId <- Ref.make[Long](0)
      nextScribbleId <- Ref.make[Long](0)
    } yield new DrawingRenderer {
      def render(drawing: Drawing) = {
        def performCommand(command: DrawCommand) = drawing.perform(command)

        val testAlternatives = div(
          children <~~ (visibleHub >>> Alternative.alternative(_ match {
            case 0 => div()
            case _ => div()
          }))
        )

        val svgBody = g(
          children <~~ drawing.events
            .map(_.body)
            .tap {
              case ScribbleStarted(scribbleId, _, _) =>
                nextScribbleId.update(n => if (n > scribbleId) n else scribbleId + 1)
              case _ =>
                ZIO.unit
            }
            .map {
              case ScribbleStarted(scribbleId, Some(start), _) =>
                val ID = scribbleId
                val startData = PathData.MoveTo(start.x, start.y)
                val points = d <-- drawing.events
                  .map(_.body)
                  .takeUntil(_ match {
                    case ScribbleDeleted(ID, _) => true
                    case _ => false
                  })
                  .collect { case ScribbleContinued(ID, points, _) => points }
                  .flatMap(points => ZStream.fromIterable(points))
                  .map { pos => PathData.LineTo(pos.x, pos.y) }
                  .mapAccum(Seq[PathData](startData)) { (seq, e) => (seq :+ e, seq :+ e) }
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

              case ScribbleDeleted(id, _) =>
                dom.document.getElementById(s"scribble${id}") match {
                  case null => None
                  case domElmt => Some(children.DeleteDOM(domElmt))
                }

              case _ =>
                None
            }
            .collect { case Some(op) => op }
        )

        div(
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
                  dom.console.log("Creating " + id)
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
                .mapZIO(performCommand)
            }
          )
        )

      }
    }
  }
}
