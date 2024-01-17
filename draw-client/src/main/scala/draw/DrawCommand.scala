package draw

import zio.Hub
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.children
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.PathData
import zio.stream.ZStream

import org.scalajs.dom

sealed trait DrawCommand

object DrawCommand {
  case class Point(x: Double, y: Double)
  case class StartScribble(id: Long, start: Point) extends DrawCommand
  case class ContinueScribble(id: Long, points: Seq[Point]) extends DrawCommand
  case class DeleteScribble(id: Long) extends DrawCommand

  def render(commands: Hub[DrawCommand]): Modifier = {
    g(
      children <~~ commands(_
        .map {
          case StartScribble(scribbleId, start) =>
            val ID = scribbleId
            val startData = PathData.MoveTo(start.x, start.y)
            val points = d <-- commands(_
              .takeUntil(_ match {
                case DeleteScribble(ID) => true
                case _ => false
              })
              .collect { case ContinueScribble(ID, points) => points }
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

          case DeleteScribble(id) =>
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
  }
}
