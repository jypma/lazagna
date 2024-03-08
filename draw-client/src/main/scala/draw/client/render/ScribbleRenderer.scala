package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.PathData

import draw.data.{ObjectState, ScribbleState}

import DrawingRenderer.dataX
import DrawingRenderer.dataY

object ScribbleRenderer {
  def make = ZIO.succeed {
    new ObjectRenderer[ScribbleState] {
      override def render(initial: ObjectState[ScribbleState], furtherEvents: Consumeable[ScribbleState]): Modifier = {
        val position = furtherEvents
          .collect { case ScribbleState(pos, _) => pos }
          .changes

        val points = d <-- furtherEvents
          .collect { case ScribbleState(_, p) => p }
          .map { p =>
            p.headOption.map(start => PathData.MoveTo(start.x, start.y)) ++
            p.tail.map(pos => PathData.LineTo(pos.x, pos.y))
          }
          .map(PathData.render)
          .changes

        g(
          cls := "scribble",
          id := s"scribble${initial.id}",
          transform <-- position.map(p => s"translate(${p.x},${p.y})"),
          dataX <-- position.map(_.x),
          dataY <-- position.map(_.y),
          path(
            points
          ),
          path(
            cls := "selectTarget editTarget",
            points
          )
        )
      }
    }
  }
}
