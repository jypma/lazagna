package draw.client.tools

import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.{PathData, SVGHelper}
import zio.lazagna.dom.{Alternative, Modifier}
import zio.stream.SubscriptionRef
import zio.{URIO, ZIO}

import draw.client.Drawing
import draw.client.render.{DrawingRenderer, RenderState, RenderedObject}
import draw.data.IconState
import draw.data.drawcommand.{CreateLink, DrawCommand}
import draw.geom.Point
import org.scalajs.dom

import DrawingRenderer.{ObjectTarget}

object LinkTool {
  case class State(srcId: String, srcPos: Point, pos: dom.SVGPoint, dst: Option[ObjectTarget])

  def apply(drawing: Drawing): URIO[RenderState, Modifier[Unit]] = for {
    state <- SubscriptionRef.make[Option[State]](None)
    renderState <- ZIO.service[RenderState]
  } yield SVGHelper { helper =>
    Modifier.all(
      Alternative.option(state) { s =>
        // Quick hack so we don't mouseover the target lineTo
        val dx = if (s.srcPos.x < s.pos.x) -1 else 1
        val dy = if (s.srcPos.y < s.pos.y) -1 else 1
        path(
          cls := "link-preview",
          d := PathData.render(Seq(PathData.MoveTo(s.srcPos.x, s.srcPos.y), PathData.LineTo(s.pos.x + dx, s.pos.y + dy)))
        )
      },
      onMouseDown(_
        .filter(_.button == 0)
        .flatMap(e => renderState.lookupForSelect(e).map((e, _)))
        .collectF { case (e, Some(RenderedObject(id, IconState(pos, _, _), _))) => (e, id, pos) }
        .flatMap { (e, srcId, srcPos) =>
          val pos = helper.screenToSvg(e)
          state.set(Some(State(srcId, srcPos, pos, None)))
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
        .flatMap((e, s) => renderState.lookupForSelect(e).map((_, s)))
        .collectF { case (Some(obj), s) if s.srcId != obj.id => (obj, s) } // No link to self!
        .zip(DrawingTools.makeUUID)
        .flatMap((obj, s, id) => {
          println("From " + s.srcId + " to " + obj)
          drawing.perform(DrawCommand(CreateLink(id, s.srcId, obj.id, None, None)))
        })
      )
    )
  }
}
