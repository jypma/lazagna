package draw.client.tools

import zio.Ref
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.SVGHelper

import draw.client.DrawingRenderer.ObjectTarget
import draw.client.{Drawing, DrawingRenderer}
import draw.data.drawcommand.{CreateLink, DrawCommand}

object LinkTool {
  case class State(src: ObjectTarget)

  def apply(drawing: Drawing) = for {
    state <- Ref.make[Option[State]](None)
  } yield SVGHelper { helper =>
    Modifier.combine(
      onMouseDown(_
        .filter(_.button == 0)
        .map(DrawingRenderer.getSelectTargetObject)
        .collectF { case Some(obj) =>
          println("from " + obj)
          obj
        }
        .flatMap(o => state.set(Some(State(o))))
      ),
      onMouseUp(_
        .zip(state.getAndSet(None))
        .collectF { case (e, Some(s)) => (e, s) }
        .map((e, s) => (DrawingRenderer.getSelectTargetObject(e), s))
        .collectF { case (Some(obj), s) => (obj, s) }
        .zip(DrawingTools.makeUUID)
        .flatMap((obj, s, id) =>
          println("From " + s.src + " to " + obj)
          drawing.perform(DrawCommand(CreateLink(id, s.src.id, obj.id, None, None)))
        )
      )
    )
  }
}
