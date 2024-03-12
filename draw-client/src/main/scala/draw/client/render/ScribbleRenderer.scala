package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.svg.PathData
import zio.lazagna.dom.{Modifier, MultiUpdate}

import draw.data.{ObjectState, ScribbleState}

import DrawingRenderer.dataX
import DrawingRenderer.dataY

object ScribbleRenderer {
  def make = for {
    rendered <- ZIO.service[RenderState]
  } yield new ObjectRenderer[ScribbleState] {
    override def render(initial: ObjectState[ScribbleState], furtherEvents: Consumeable[ScribbleState]): Modifier = Modifier.unwrap {
      for {
        state <- MultiUpdate.make[ScribbleState]
      } yield {
        def pointData = state { s =>
          val p = s.points
          d := PathData.render(
            p.headOption.map(start => PathData.MoveTo(start.x, start.y)) ++
              p.tail.map(pos => PathData.LineTo(pos.x, pos.y))
          )
        }

        g(
          cls := "scribble",
          id := s"scribble${initial.id}",
          state { s =>
            val p = s.position
            transform.set(s"translate(${p.x},${p.y})") *>
            dataX.set(p.x) *>
            dataY.set(p.y)
          },
          path(
            pointData
          ),
          path(
            cls := "selectTarget editTarget",
            pointData
          ),
          thisElementAs { element =>
            furtherEvents
              .via(state.pipeline)
              .tap { state => rendered.notifyRendered(RenderedObject(initial.id, state, element)) }
          }
        )
      }
    }
  }
}
