package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.svg.PathData
import zio.lazagna.dom.{MultiUpdate}

import draw.data.{ObjectState, ScribbleState}

object ScribbleRenderer {
  def make = for {
    rendered <- ZIO.service[RenderState]
  } yield new ObjectRenderer[ScribbleState] {
    override def render(initial: ObjectState[ScribbleState], furtherEvents: Consumeable[ObjectState[ScribbleState]]) = for {
      state <- MultiUpdate.make[ObjectState[ScribbleState]]
      res <- {
        def pointData = state { s =>
          val p = s.body.points
          d := PathData.render(
            p.headOption.map(start => PathData.MoveTo(start.x, start.y)) ++
              p.tail.map(pos => PathData.LineTo(pos.x, pos.y))
          )
        }

        g(
          cls := "scribble",
          id := s"scribble${initial.id}",
          state { s =>
            val p = s.body.position
            transform.set(s"translate(${p.x},${p.y})")
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
              .tap { state => rendered.notifyRendered(state, element) }
              .consume
          }
        )
      }
    } yield res
  }
}
