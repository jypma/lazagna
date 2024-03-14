package draw.client.tools

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Children, Modifier}
import zio.stream.SubscriptionRef
import zio.{URIO, ZIO}

import draw.client.Drawing
import draw.client.render.{RenderState, DrawingRenderer}
import draw.data.drawcommand.{DeleteObject, DrawCommand, MoveObject}
import draw.data.point.Point
import draw.data.{IconState, Moveable, ObjectState}
import org.scalajs.dom

import DrawingRenderer.{iconSize}
import draw.data.drawcommand.LabelObject

object SelectTool {
  private case class State(selection: Set[ObjectState[Moveable]], dragStart: Point)

  def make(drawing: Drawing, dialogs: Children, keyboard: Children): URIO[RenderState,Modifier[Unit]] = for {
    state <- SubscriptionRef.make[Option[State]](None)
    editingLabel <- SubscriptionRef.make[Option[(String, IconState)]](None)
    addToSelection <- SubscriptionRef.make(false)
    removeFromSelection <- SubscriptionRef.make(false)
    renderState <- ZIO.service[RenderState]
  } yield SVGHelper { helper =>
    def doSelect(event: dom.MouseEvent) = {
      val pos = helper.screenToSvg(event)
      for {
        target <- renderState.lookupForSelect(event).map(_.map(_.id).toSet)
        _ <- addToSelection.get.zip(removeFromSelection.get).flatMap { (adding, removing) =>
          if (adding) {
            drawing.selection.update(_ ++ target)
          } else if (removing) {
            drawing.selection.update(_ -- target)
          } else {
            drawing.selection.set(target)
          }
        }
      } yield ()
    }

    Modifier.all(
      Alternative.mountOne(editingLabel) {
        case Some(target) =>
          dialogs.child { _ =>
            val pos = helper.svgToScreen(target._2.position.move(-iconSize / 2, iconSize * 0.45))
            val bounds = helper.svgToScreen(target._2.position.move(iconSize / 2, -iconSize / 2))
            val width = bounds.x - pos.x
            val close = editingLabel.set(None)
            div(
              cls := "label-input",
              style := s"left: ${pos.x - width * 1}px; top: ${pos.y}px;",
              input(
                style := s"width: ${width * 3}px; font-size:${width * 45 / 178}px",
                typ := "text",
                placeholder := "Enter label...",
                focusNow,
                value <-- drawing.objectState(target._1).map(_.body).collect { case IconState(_,_,label) => label },
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
      Alternative.mountOne(state) {
        case Some(s) if s.selection.exists(_.body.isInstanceOf[IconState]) =>
          val icon = s.selection.head
          println("Adding child")
          keyboard.child { _ =>
            DrawingTools.keyboardAction("t", "Edit label", editingLabel.set(Some(icon.id, icon.body.asInstanceOf[IconState])))
          }
      },
      onMouseDown(_
        .filter { e => (e.buttons & 1) != 0 }
        .tap(doSelect)
        .zip(drawing.currentSelectionState.map { _.collect {
          case s@ObjectState(_,_,_,_: Moveable) => s.asInstanceOf[ObjectState[Moveable]]
        }})
        .collectF {
          case (event, selection) if !selection.isEmpty =>
            val pos = helper.screenToSvg(event)
            val res = Some(State(selection, Point(pos.x, pos.y)))
            println("Selection: " + selection)
            res
          case _ =>
            println("No selection.")
            None
        }
        .flatMap(state.set)
        .tap(_ => editingLabel.set(None))
      ),
      onMouseMove(_
        .filter { e => (e.buttons & 1) != 0 }
        .zip(state.get)
        .collectF {
          case (event, Some(state)) => (state, helper.screenToSvg(event))
        }
        .flatMap { (state, pos) =>
          ZIO.collectAll(state.selection.map { obj =>
            val x = obj.body.position.x + pos.x - state.dragStart.x
            val y = obj.body.position.y + pos.y - state.dragStart.y
            drawing.perform(DrawCommand(MoveObject(obj.id, Some(Point(x, y)))))
          })
        }
      ),
      keyboard.child { _ =>
        DrawingTools.keyboardToggle("a", "Add to sticky selection", addToSelection, b => removeFromSelection.set(false).when(b))
      },
      keyboard.child { _ =>
        DrawingTools.keyboardToggle("r", "Remove from sticky selection", removeFromSelection, b => addToSelection.set(false).when(b))
      },
      Alternative.mountOne(drawing.selection) { selection =>
        if (selection.isEmpty) Modifier.empty else {
          keyboard.child { _ =>
            DrawingTools.keyboardAction("Delete", "Delete items",
              ZIO.collectAll(selection.map { id =>
                drawing.perform(DrawCommand(DeleteObject(id)))
              }).unit
            )
          }
        }
      }
    )
  }
}
