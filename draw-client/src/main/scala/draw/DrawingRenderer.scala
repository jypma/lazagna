package draw

import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.children
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.{PathData, SVGOps}
import zio.stream.ZStream
import zio.{Ref, ZIO, ZLayer}

import draw.data.drawevent.{ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import org.scalajs.dom

trait DrawingRenderer {
  def render: Modifier
}

object DrawingRenderer {
  val live = ZLayer.fromZIO {
    for {
      drawing <- ZIO.service[Drawing]
      nextScribbleId <- Ref.make[Long](0)
    } yield new DrawingRenderer {
      def performCommand(command: DrawCommand) = drawing.perform(command).catchAll { failure =>
        ZIO.succeed {
          dom.console.log(failure.message)
        }
      }

      val svgBody = g(
        children <~~ drawing.events(_
          .map(_.body)
          .map {
            case ScribbleStarted(scribbleId, Some(start), _) =>
              val ID = scribbleId
              val startData = PathData.MoveTo(start.x, start.y)
              val points = d <-- drawing.events(_
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
              )

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
      )

      val svgElem = div(
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
            Modifier.combine(
              onMouseDown(_
                .filter { e => (e.buttons & 1) != 0 }
                .filter(ev => !ev.getModifierState("Alt"))
                .mapZIO(ev => nextScribbleId.get.map { id =>
                  val pos = helper.getClientPoint(ev)
                  DrawCommand.StartScribble(id, DrawCommand.Point(pos.x, pos.y))
                })
                .mapZIO(performCommand)
              ).run,
              onMouseDown.merge(onMouseMove)(_
                .filter { e => (e.buttons & 1) != 0 }
                .filter { ev => ev.getModifierState("Alt") }
                .map(_.target)
                .collect { case elem: dom.Element => elem }
                .map(_.parentNode)
                .collect {
                  case parent:dom.Element if parent.id.startsWith("scribble") =>
                    val id = parent.id.substring("scribble".length).toLong
                    DrawCommand.DeleteScribble(id)
                }
                .mapZIO(performCommand)
              ).run,
              onMouseMove(_
                .filter { e => (e.buttons & 1) != 0 }
                .mapZIO(ev => nextScribbleId.get.map(id =>
                  val pos = helper.getClientPoint(ev)
                    DrawCommand.ContinueScribble(id, Seq(DrawCommand.Point(pos.x, pos.y)))
                ))
                .mapZIO(performCommand)
              ).run,
              onMouseUp(_
                .mapZIO(_ => nextScribbleId.update(_ + 1))
              ).run
            )
          }
        )
      )

      override def render = svgElem
    }
  }
}
