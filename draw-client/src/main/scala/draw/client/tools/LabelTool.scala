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

import draw.client.Drawing
import draw.client.render.DrawingRenderer
import draw.data.IconState
import draw.data.drawcommand.{DrawCommand, LabelObject}

import DrawingRenderer.{ObjectTarget, iconSize}

object LabelTool {
  def make(drawing: Drawing, dialogs: Children, keyboard: Children) = for {
    selected <- SubscriptionRef.make[Option[ObjectTarget]](None)
  } yield {
    SVGHelper { helper =>
      Modifier.combine(
        Alternative.mountOne(selected) {
          case None => Modifier.empty
          case Some(target) =>
            dialogs.addChild { _ =>
              val pos = helper.svgToScreen(target.position.move(-iconSize / 2, iconSize * 0.45))
              val bounds = helper.svgToScreen(target.position.move(iconSize / 2, -iconSize / 2))
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
                  value <-- drawing.objectState(target.id).map(_.body).collect { case IconState(_,_,label) => label },
                  onInput.asTargetValue(_.flatMap { text =>
                    drawing.perform(DrawCommand(LabelObject(target.id, text)))
                  })
                ),
                keyboard.addChild { _ =>
                  DrawingTools.keyboardAction("Escape", "Close dialog", close)
                },
                keyboard.addChild { _ =>
                  DrawingTools.keyboardAction("Enter", "Close dialog", close)
                }
              )
            }
        },
        onMouseDown(_
          .filter(_.button == 0)
          .flatMap { e =>
            selected.set(DrawingRenderer.getEditTargetObject(e))
          }
        )
      )
    }
  }
}
