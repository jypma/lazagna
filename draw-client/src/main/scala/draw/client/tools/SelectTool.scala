package draw.client.tools

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Children, Modifier}
import zio.stream.SubscriptionRef
import zio.{URIO, ZIO}

import draw.client.Drawing
import draw.client.render.RenderState
import draw.data.drawcommand.{DeleteObject, DrawCommand, EditLink, LabelObject, MoveObject}
import draw.data.{IconState, LinkState, Moveable, ObjectState, SymbolRef}
import draw.geom.Point
import org.scalajs.dom

import IconState.mediumSize
import draw.client.render.RenderedObject

object SelectTool {
  private case class State(selection: Set[(String, Moveable)], dragStart: Point)

  def make(drawing: Drawing, dialogs: Children, keyboard: Children): URIO[RenderState,Modifier[Unit]] = for {
    state <- SubscriptionRef.make[Option[State]](None)
    editingLabel <- SubscriptionRef.make[Option[(String, IconState)]](None)
    editingLink <- SubscriptionRef.make[Option[(String, LinkState)]](None)
    addToSelection <- SubscriptionRef.make(false)
    removeFromSelection <- SubscriptionRef.make(false)
    renderState <- ZIO.service[RenderState]
  } yield SVGHelper { helper =>
    helper.measurer(
      text(
        cls := "label",
        style := "opacity: 100"
      )
    ) { measureLabel =>
    def doSelect(targets: Set[String]) = addToSelection.get.zip(removeFromSelection.get).flatMap { (adding, removing) =>
      if (adding) {
        renderState.selectAlso(targets)
      } else if (removing) {
        renderState.unselect(targets)
          } else {
        renderState.selectOnly(targets)
      }
    }

    def handleSelect(event: dom.MouseEvent) = {
      val pos = helper.screenToSvg(event)
      renderState.lookupForSelect(event).map(_.map(_.id).toSet).flatMap(doSelect)
    }

    def expandSelect(direction: Double => Boolean) = for {
      newSelection <- renderState.expandSelection(direction).map(_.map(_.id).toSet)
      adding <- addToSelection.get
      removing <- removeFromSelection.get
      _ <- if (adding || removing || !newSelection.isEmpty) doSelect(newSelection) else ZIO.unit
    } yield ()

    def headState(set: Iterable[RenderedObject]) = set.headOption.map(_.state)

    Modifier.all(
      g(
        cls := "selection-crosshair",
        renderState.selectionBoundingBox.mapZIO { box =>
          val pos = box.map(_.middle).getOrElse(Point(100,100))
          (transform := s"translate(${pos.x},${pos.y})") *> (visibility := (if (box.isEmpty) "hidden" else "visible"))
        }.consume,
        use(
          href := SymbolRef.plus.href,
          width := 10,
          height := 10,
          x := -5,
          y := -5
        )
      ),
      keyboard.child { _ =>
        div(
          DrawingTools.keyboardAction("ArrowLeft", "Select to the left", expandSelect(RenderState.Direction.left)),
          DrawingTools.keyboardAction("ArrowRight", "Select to the right", expandSelect(RenderState.Direction.right)),
          DrawingTools.keyboardAction("ArrowUp", "Select upwards", expandSelect(RenderState.Direction.up)),
          DrawingTools.keyboardAction("ArrowDown", "Select downwards", expandSelect(RenderState.Direction.down))
        )
      },
      Alternative.option(editingLabel) { target =>
        dialogs.child { _ =>
          val pos = helper.svgToScreen(target._2.position.move(-mediumSize / 2, mediumSize * 0.45))
          val bounds = helper.svgToScreen(target._2.position.move(mediumSize / 2, -mediumSize / 2))
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
              value <-- drawing.objectState(target._1).map(_.body).collect { case IconState(_,_,label,_,_) => label },
              onInput.asTargetValue(_.flatMap { text =>
                measureLabel.boundingBox(textContent := text).flatMap { box =>
                  drawing.perform(DrawCommand(LabelObject(target._1, text, box.width, box.height, box.origin.y)))
                }
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
      Alternative.option(editingLink) { (linkId, s) =>
        val lengths = Seq(
          ("∅", "Manual"),
          ("⎼", "Short"),
          ("⎯", "Medium"),
          ("⎯⎯", "Long")
        )

        val directions = Seq(
          ("←", "left", 270),
          ("↖", "up and left", 225),
          ("↑", "up", 180),
          ("↗", "up and right", 135),
          ("→", "right", 90),
          ("↘", "down and right", 45),
          ("↓", "down", 360),
          ("↙", "down and left", 315),
        )
        dialogs.child { _ =>
          div(
            cls := "dialog",
            div (
              cls := "link-dialog",
              div(textContent := "Length:"),
              div(
                cls := "lengths",
                lengths.zipWithIndex.map { case ((icon, desc), idx) =>
                  div(
                    cls := "length",
                    input(typ := "radio", name := "length", id := s"length${idx}",
                      checked <-- drawing.follow[LinkState](linkId).map(_.body.preferredDistance.getOrElse(0) == idx)
                    ),
                    div(
                      label(`for` := s"length${idx}", textContent := icon, title := desc),
                      onClick(_.tap { _ =>
                        drawing.perform(DrawCommand(EditLink(linkId, preferredDistance = Some(idx))))
                      })
                    )
                  )
                }
              ),
              div(textContent := "Direction:"),
              div(
                cls := "direction",
                input(
                  typ := "text",
                  placeholder := "∅",
                  title := "Preferred angle, or empty for no preference",
                  value <-- drawing.follow[LinkState](linkId)
                    .map(_.body.preferredAngle.filter(_ != 0).map(_.toString).getOrElse("")),
                  onInput.asTargetValue(_
                    .map(_.toIntOption)
                    .collectF {
                      case Some(i) if i >= 0 && i <= 360 => i
                      case _ => 0
                    }
                    .flatMap { i =>
                      drawing.perform(DrawCommand(EditLink(linkId, preferredAngle = Some(i))))
                    }
                  )
                ),
                span(textContent := " degrees")
              ),
              div(),
              div(
                cls := "direction-presets",
                directions.map { case (s, desc, angle) =>
                  div(
                    cls := "preset",
                    textContent := s,
                    title := s"Set direction to ${desc}",
                    onClick(_.tap { _ =>
                      drawing.perform(DrawCommand(EditLink(linkId, preferredAngle = Some(angle))))
                    })
                  )
                }
              )
            ),
            keyboard.child { _ =>
              DrawingTools.keyboardAction("Escape", "Close dialog", editingLink.set(None))
            }
          )
        }
      },
      Alternative.mountOne(renderState.selection) {
        headState(_) match {
          case Some(ObjectState(id,_,_,icon:IconState)) =>
            keyboard.child { _ =>
              DrawingTools.keyboardAction("t", "Edit label", editingLabel.set(Some(id, icon)))
            }
          case Some(ObjectState(id,_,_,state:LinkState)) =>
            keyboard.child { _ =>
              DrawingTools.keyboardAction("e", "Edit link", editingLink.set(Some(id, state)))
            }
          case other =>
            println("Not editable: " + other)
            Modifier.empty
        }
      },
      onMouseDown(_
        .filter { e => (e.buttons & 1) != 0 }
        .tap(handleSelect)
        .zip(renderState.currentSelectionState.map { _.map(_.state).collect {
          case ObjectState(id,_,_,state:Moveable) =>
            (id, state)
        }})
        .collectF {
          case (event, selection) if !selection.isEmpty =>
            val pos = helper.screenToSvg(event)
            Some(State(selection, Point(pos.x, pos.y)))
          case _ =>
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
          ZIO.collectAll(state.selection.map { (id, obj) =>
            val x = obj.position.x + pos.x - state.dragStart.x
            val y = obj.position.y + pos.y - state.dragStart.y
            drawing.perform(DrawCommand(MoveObject(id, Some(draw.data.point.Point(x, y)))))
          })
        }
      ),
      onMouseUp(_
        .tap(_ => state.set(None))
      ),
      keyboard.child { _ =>
        div(
          DrawingTools.keyboardToggle("a", "Add to sticky selection", addToSelection, b => removeFromSelection.set(false).when(b)),
          DrawingTools.keyboardToggle("r", "Remove from sticky selection", removeFromSelection, b => addToSelection.set(false).when(b))
        )
      },
      Alternative.mountOne(renderState.selectionIds) {
        case selection if !(selection.isEmpty) =>
          keyboard.child { _ =>
            DrawingTools.keyboardAction("Delete", "Delete items",
              ZIO.collectAll(selection.map { id =>
                drawing.perform(DrawCommand(DeleteObject(id)))
              }).unit
            )
          }
      }
    )
    }
  }
}
