package draw.client.tools

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Children, Modifier}
import zio.stream.SubscriptionRef
import zio.{Ref, URIO, ZIO}

import draw.client.Drawing
import draw.client.render.RenderState
import draw.data.drawcommand.{DeleteObject, DrawCommand, MoveObject}
import draw.data.point.Point
import draw.data.{Moveable, ObjectState}
import org.scalajs.dom

object SelectTool {
  case class MoveState(selection: Set[ObjectState[Moveable]], start: Point)

  def make(drawing: Drawing, keyboard: Children): URIO[RenderState,Modifier[Unit]] = for {
    moveState <- Ref.make[Option[MoveState]](None)
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
        onMouseDown(_
          .filter { e => (e.buttons & 1) != 0 }
          .tap(doSelect)
          .zip(drawing.currentSelectionState.map { _.collect {
            case s@ObjectState(_,_,_,_: Moveable) => s.asInstanceOf[ObjectState[Moveable]]
          }})
          .collectF {
            case (event, selection) if !selection.isEmpty =>
              println("Preparing to move.")
              val pos = helper.screenToSvg(event)
              Some(MoveState(selection, Point(pos.x, pos.y)))
          }
          .flatMap(moveState.set)
        ),
        onMouseUp(_.flatMap { _ => moveState.set(None) }),
        onMouseMove(_
          .zip(moveState.get)
          .collectF {
            case (event, Some(state)) => (state, helper.screenToSvg(event))
          }
          .flatMap { (state, pos) =>
            ZIO.collectAll(state.selection.map { obj =>
              val x = obj.body.position.x + pos.x - state.start.x
              val y = obj.body.position.y + pos.y - state.start.y
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
