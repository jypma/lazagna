package draw.client.tools

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Children, Modifier}
import zio.stream.SubscriptionRef
import zio.{URIO, ZIO}

import draw.client.Drawing
import draw.client.render.{DrawingRenderer, RenderState}
import draw.data.drawcommand.{DrawCommand, LabelObject}
import draw.data.{IconState, ObjectState}

import DrawingRenderer.{iconSize}

object LabelTool {
  def make(drawing: Drawing, dialogs: Children, keyboard: Children): URIO[RenderState, Modifier[Unit]] = for {
    selected <- SubscriptionRef.make[Option[(String, IconState)]](None)
    renderState <- ZIO.service[RenderState]
  } yield SVGHelper { helper =>
    Modifier.all(
      Alternative.mountOne(selected) {
        case Some(target) =>
          dialogs.child { _ =>
            val pos = helper.svgToScreen(target._2.position.move(-iconSize / 2, iconSize * 0.45))
            val bounds = helper.svgToScreen(target._2.position.move(iconSize / 2, -iconSize / 2))
            val width = bounds.x - pos.x
            val close = selected.set(None)
            div(
              cls := "label-input",
              style := s"left: ${pos.x - width * 1}px; top: ${pos.y}px;",
              input(
                style := s"width: ${width * 3}px; font-size:${width * 45 / 178}px",
                typ := "text",
                placeholder := "Enter label...",
                focusNow,
                value <-- drawing.objectState(target._1).map(_.body).collect { case IconState(_,_,label,_) => label },
                onInput.asTargetValue(_.flatMap { text =>
                  drawing.perform(DrawCommand(LabelObject(target._1, text)))
                })
              ),
              keyboard.child { _ =>
                DrawingTools.keyboardAction("Escape", "Close dialog", close)
              },
              keyboard.child { _ =>
                DrawingTools.keyboardAction("Enter", "Close dialog", close)
              }
            )
          }
      },
      onMouseDown(_
        .filter(_.button == 0)
        .flatMap { e =>
          renderState.lookupForEdit(e).flatMap(o => selected.set(o.map(_.state).collect {
            case ObjectState(id,_,_,state:IconState) => (id, state)
          }))
        }
      )
    )
  }
}
