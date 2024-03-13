package draw.client.tools

import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.{PathData, SVGHelper}
import zio.lazagna.dom.{Alternative, Modifier}
import zio.stream.SubscriptionRef
import zio.UIO

import draw.client.Drawing
import draw.client.render.DrawingRenderer
import draw.data.drawcommand.{CreateLink, DrawCommand}
import org.scalajs.dom

import DrawingRenderer.{ObjectTarget}

object LinkTool {
  case class State(src: ObjectTarget, pos: dom.SVGPoint, dst: Option[ObjectTarget])

  def apply(drawing: Drawing): UIO[Modifier[Unit]] = for {
    state <- SubscriptionRef.make[Option[State]](None)
  } yield SVGHelper { helper =>
    Modifier.all(
      Alternative.option(state) { s =>
        path(
          cls := "link-preview",
          d := PathData.render(Seq(PathData.MoveTo(s.src.position.x, s.src.position.y), PathData.LineTo(s.pos.x, s.pos.y)))
        )
      },
      onMouseDown(_
        .filter(_.button == 0)
        .map(e => (e, DrawingRenderer.getSelectTargetObject(e)))
        .collectF { case (e, Some(obj)) => (e, obj) }
        .flatMap { (e, obj) =>
          val pos = helper.screenToSvg(e)
          state.set(Some(State(obj, pos, None)))
        }
      ),
      onMouseMove(_
        .zip(state.get)
        .collectF { case (e, Some(s)) => (e, s) }
        .flatMap { (e, s) =>
          val pos = helper.screenToSvg(e)
          state.set(Some(s.copy(pos = pos)))
        }
      ),
      onMouseUp(_
        .zip(state.getAndSet(None))
        .collectF { case (e, Some(s)) => (e, s) }
        .map((e, s) => (DrawingRenderer.getSelectTargetObject(e), s))
        .collectF { case (Some(obj), s) if s.src.id != obj.id => (obj, s) } // No link to self!
        .zip(DrawingTools.makeUUID)
        .flatMap((obj, s, id) =>
          println("From " + s.src + " to " + obj)
            drawing.perform(DrawCommand(CreateLink(id, s.src.id, obj.id, None, None)))
        )
      )
    )
  }
}
