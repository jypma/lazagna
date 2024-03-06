package draw.client.tools

import java.util.Base64

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Children, Element, Modifier}
import zio.stream.SubscriptionRef
import zio.{Clock, Hub, Random, Ref, Scope, UIO, ZIO, ZLayer}

import draw.client.Drawing._
import draw.client.DrawingRenderer.ObjectTarget
import draw.client.{Drawing, DrawingRenderer, SymbolIndex}
import draw.data.drawcommand.{ContinueScribble, CreateIcon, DeleteObject, DrawCommand, LabelObject, MoveObject, StartScribble}
import draw.data.point.Point
import draw.data.{IconState, Moveable, ObjectState, SymbolRef}
import org.scalajs.dom

trait DrawingTools {
  def renderKeyboard: Modifier
  def renderHandlers: Modifier
  def renderToolbox: Modifier
  def currentToolName: Consumeable[String]
}

object DrawingTools {
  private def keyboardAction(key: String, description: String, execute: => ZIO[Scope, Nothing, Unit],
    extraCSS: Option[Consumeable[String]] = None): Element[dom.Element] = {
    val keyName = key match {
      case "Escape" => "⎋"
      case "Enter" => "↵"
      case "Delete" => "⌦"
      case s => s.toUpperCase()
    }

    val css = extraCSS.map(css => cls <-- combineSpaced("hint", css)).getOrElse(cls := "hint")

    div(
      css,
      div(
        cls := "key",
        textContent := keyName,
        title := key,
        onClick(_.flatMap(_ => execute))
      ),
      div(cls := "description", textContent := description),
      onKeyDown.forWindow(_.filter { e =>
        // The key is the one we're listening on:
        (e.key == key) &&
        // It's a function key, OR it's a normal key and we're not on an input element:
        (e.key.length() > 1 || !e.target.isInstanceOf[dom.HTMLInputElement])
      }.flatMap{_ =>
        execute
      })
    )
  }

  private def keyboardToggle(key: String, description: String, target: SubscriptionRef[Boolean],
    onChange: Boolean => UIO[Any] = {_ => ZIO.unit}): Element[dom.Element] = {
    keyboardAction(key, description, target.update(toB => !toB), Some(target.tap(onChange).map(a => if (a) "active" else "")))
  }

  private case class Tool(key: String, name: String, hint: String, icon: String, render: Modifier)

  val live = ZLayer.scoped {
    for {
      drawing <- ZIO.service[Drawing]
      dialogs <- ZIO.service[Children]
      index <- ZIO.service[SymbolIndex]
      keyboard <- Children.make
      iconTool <- icon(drawing, dialogs, keyboard, index)
      labelTool <- labelTool(drawing, dialogs, keyboard)
      selectTool <- selectTool(drawing, keyboard)
      linkTool <- LinkTool(drawing)
      tools = Seq(
        Tool("s", "select", "Select, move and adjust existing objects", "⛶", selectTool),
        Tool("p", "pencil", "Add pencil strokes", "✏️", pencil(drawing)),
        Tool("i", "icon", "Add icons", "🚶", iconTool),
        Tool("t", "label", "Add labels", "🏷️", labelTool),
        Tool("l", "link", "Add links", "🔗", linkTool),
      )
      selectedTool <- SubscriptionRef.make(tools(0))
      common <- commonTools(drawing, keyboard, selectedTool, tools)
    } yield new DrawingTools {
      override def currentToolName = selectedTool.map(_.name)

      override val renderHandlers = Alternative.mountOne(selectedTool) { t => Modifier.combine(common, t.render) }

      override val renderToolbox = div(
        cls := "toolbox",
        tools.map { tool =>
          div(
            cls := "tool",
            input(`type` := "radio", name := "tool", id := s"tool-${tool.name}",
              checked <-- selectedTool.map { _ == tool }
            ),
            div(
              label (`for` := s"tool-${tool.name}", title := tool.hint, textContent := tool.icon),
              onClick(_.as(tool)) --> selectedTool
            )
          )
        }
      )

      override val renderKeyboard =
        div(
          cls := "keyboard-hints",
          keyboard.render
        )
    }
  }

  /** Creates a base64-encoded, version 7 time-based UUID, truncated to 22 characters. */
  def makeUUID: UIO[String] = for {
    now <- Clock.instant
    random <- Random.nextBytes(10)
  } yield {
    val time = BigInt(now.toEpochMilli()).toByteArray.reverse.padTo(6, 0:Byte).reverse
    val uuid = time ++ random.toArray
    uuid(6) = (0x70 | (uuid(6) & 0x0F)).toByte // Set version 7
    uuid(8) = (0x80 | (uuid(8) & 0x3F)).toByte // Set variant 0b10
    Base64.getEncoder().encodeToString(uuid).take(22) // Remove the trailing ==
  }

  private def commonTools(drawing: Drawing, keyboard: Children, selectedTool: SubscriptionRef[Tool], tools:Seq[Tool]) = for {
    dragStart <- Ref.make(Point(0,0))
  } yield {
    SVGHelper { helper =>
      Modifier.unwrap(
        for {
          exporter <- Exporter.make(helper.svg).provideLayer(ZLayer.succeed(drawing))
        } yield Modifier.combine(
          onMouseDown(_
            .filter { e => (e.buttons & 4) != 0 }
            .flatMap { event =>
              val pos = helper.screenToLocal(event)
              dragStart.set(Point(pos.x, pos.y))
            }
          ),
          onMouseMove(_
            .filter { e => (e.buttons & 4) != 0 }
            .flatMap { event =>
              val pos = helper.screenToLocal(event)
              for {
                start <- dragStart.get
                _ <- drawing.viewport.update(_.pan(start.x - pos.x, start.y - pos.y))
              } yield ()
            }),
          onWheel(_
            .flatMap { event =>
              val factor = event.deltaMode match {
                case 0 => (event.deltaY * 0.01) + 1
                case 1 => (event.deltaY * 0.01) + 1
                case _ => (event.deltaY * 0.01) + 1
              }
              val pos = helper.screenToLocal(event)
              drawing.viewport.update(_.zoom(factor, pos.x, pos.y))
            }),
          keyboard.addChild { _ =>
            div(
              keyboardAction("?", "Tutorial (MMB to pan, wheel to zoom)", ZIO.unit),
              keyboardAction("f", "Fit into view",
                drawing.viewport.set(Drawing.Viewport.fit(helper.svg.getBBox()))
              ),
              keyboardAction("v", "Export as SVG", exporter.triggerExport.catchAll(ZIO.debug(_))),
              tools.map { tool =>
                keyboardAction(tool.key, tool.hint, selectedTool.set(tool))
              }
            )
          }
        )
      )
    }
  }

  case class MoveState(selection: Set[ObjectState[Moveable]], start: Point)
  def selectTool(drawing: Drawing, keyboard: Children) = for {
    moveState <- Ref.make[Option[MoveState]](None)
    addToSelection <- SubscriptionRef.make(false)
    removeFromSelection <- SubscriptionRef.make(false)
  } yield {
    SVGHelper { helper =>
      def doSelect(event: dom.MouseEvent) = {
        val pos = helper.screenToLocal(event)
        val target = DrawingRenderer.getSelectTargetObject(event).map(_.id).toSet
        addToSelection.get.zip(removeFromSelection.get).flatMap { (adding, removing) =>
          if (adding) {
            drawing.selection.update(_ ++ target)
          } else if (removing) {
            drawing.selection.update(_ -- target)
          } else {
            drawing.selection.set(target)
          }
        }
      }

      Modifier.combine(
        onMouseDown(_
          .filter { e => (e.buttons & 1) != 0 }
          .tap(doSelect)
          .zip(drawing.currentSelectionState.map { _.collect {
            case s@ObjectState(_,_,_,_: Moveable) => s.asInstanceOf[ObjectState[Moveable]]
          }})
          .collectF {
            case (event, selection) if !selection.isEmpty =>
              println("Preparing to move.")
              val pos = helper.screenToLocal(event)
              Some(MoveState(selection, Point(pos.x, pos.y)))
          }
          .flatMap(moveState.set)
        ),
        onMouseUp(_.flatMap { _ => moveState.set(None) }),
        onMouseMove(_
          .zip(moveState.get)
          .collectF {
            case (event, Some(state)) => (state, helper.screenToLocal(event))
          }
          .flatMap { (state, pos) =>
            ZIO.collectAll(state.selection.map { obj =>
              val x = obj.body.position.x + pos.x - state.start.x
              val y = obj.body.position.y + pos.y - state.start.y
              drawing.perform(DrawCommand(MoveObject(obj.id, Some(Point(x, y)))))
            })
          }
        ),
        keyboard.addChild { _ =>
          keyboardToggle("a", "Add to sticky selection", addToSelection, b => removeFromSelection.set(false).when(b))
        },
        keyboard.addChild { _ =>
          keyboardToggle("r", "Remove from sticky selection", removeFromSelection, b => addToSelection.set(false).when(b))
        },
        Alternative.mountOne(drawing.selection) { selection =>
          if (selection.isEmpty) Modifier.empty else {
            keyboard.addChild { _ =>
              keyboardAction("Delete", "Delete items",
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

  def pencil(drawing: Drawing): Modifier = Modifier.unwrap(for {
    currentScribbleId <- Ref.make[Option[String]](None)
    startPoint <- Ref.make[Option[Point]](None)
    count <- Ref.make(0)
  } yield SVGHelper { helper =>
    onMouseDown(_
      .filter { e => (e.buttons & 1) != 0 }
      .flatMap { ev =>
        val pos = helper.screenToLocal(ev)
        startPoint.set(Some(Point(pos.x, pos.y))) *>
          count.set(0) *>
          makeUUID.flatMap(id => currentScribbleId.set(Some(id)))
      }
      .drain
    ).merge(
      onMouseMove(_
        .filter { e => (e.buttons & 1) != 0 }
        .flatMap(ev => currentScribbleId.get.map((_, ev)))
        .collectF { case (Some(id), ev) => (id, ev) }
        .zip(count.updateAndGet(_ + 1))
        .collectF { case (id, ev, count) if count < 200 => (id, ev) }
        .flatMap((id, ev) => startPoint.get.map((id, _, ev)))
        .flatMap { (id, start, event) =>
          val pos = helper.screenToLocal(event)
          val point = Point(pos.x, pos.y)

          if (start.isDefined) {
            startPoint.set(None).as(DrawCommand(StartScribble(id, Seq(start.get, point))))
          } else {
            ZIO.succeed(DrawCommand(ContinueScribble(id, Seq(point))))
          }
        }
      )
    ).merge(
      onMouseUp(_.flatMap(ev => currentScribbleId.set(None)).drain)
    )(_.flatMap(drawing.perform _))
  })

  private val iconSize = 64

  private def icon(drawing: Drawing, dialogs: Children, keyboard: Children, index: SymbolIndex) = for {
    searchResult <- Hub.bounded[SymbolIndex.Result](1)
    selectedIcon <- SubscriptionRef.make(SymbolRef.person)
    cursorPos <- SubscriptionRef.make[Option[dom.SVGPoint]](None)
  } yield {
    val selectDialog = dialogs.child { close =>
      div(
        cls := "dialog icon-dialog",
        div(
          div(
            cls := "results",
            Alternative.mountOne(searchResult) {
              _.symbols.map { symbol =>
                svg(
                  cls := "result",
                  tabindex := 0,
                  use(
                    svgTitle(textContent := symbol.name),
                    href := symbol.href,
                    cls := "icon",
                    width := 24,
                    height := 24
                  ),
                  onClick.merge(onKeyDown(_.filter(_.key == "Enter")))(_.flatMap { _ =>
                    selectedIcon.set(symbol) *> close
                  })
                    // TODO: "Enter" on the input itself switches focus to the first icon, and activates letter-overlay shortcuts for the first 36 matches.
                )
              }
            }
          ),
          div(
            input(typ := "text", placeholder := "Search icon...", list := "icon-dialog-list", focusNow,
              onInput.asTargetValue(_.flatMap { text =>
                index.lookup(text).flatMap(searchResult.publish)
              })
            ),
            datalist(id := "icon-dialog-list",
              Alternative.mountOne(searchResult) { _.completions.map { s => option(value := s) } }
            )
          ),
        ),
        keyboard.addChild { _ =>
          keyboardAction("Escape", "Close dialog", close)
        }
      )
    }

    SVGHelper { helper =>
      Modifier.combine(
        onMouseMove(_.flatMap { e =>
          cursorPos.set(Some(helper.screenToLocal(e)))
        }),
        keyboard.addChild { _ =>
          keyboardAction("t", "Select icon", selectDialog)
        },
        onMouseDown(_
          .filter(_.button == 0)
          .flatMap { e =>
            for {
              symbol <- selectedIcon.get
              id <- makeUUID
              pos = helper.screenToLocal(e)
              _ <- drawing.perform(DrawCommand(CreateIcon(id, Point(pos.x, pos.y), symbol.category.name, symbol.name)))
            } yield {}
          }),
        use(
          x <-- cursorPos.map(p => (p.map(_.x).getOrElse(-100000.0) - iconSize / 2).toString),
          y <-- cursorPos.map(p => (p.map(_.y).getOrElse(-100000.0) - iconSize / 2).toString),
          width := iconSize,
          height := iconSize,
          cls := "icon-preview",
          href <-- selectedIcon.map(_.href),
        )
      )
    }
  }

  private def labelTool(drawing: Drawing, dialogs: Children, keyboard: Children) = for {
    selected <- SubscriptionRef.make[Option[ObjectTarget]](None)
  } yield {
    SVGHelper { helper =>
      Modifier.combine(
        Alternative.mountOne(selected) {
          case None => Modifier.empty
          case Some(target) =>
            dialogs.addChild { _ =>
              val pos = helper.localToScreen(Point(target.position.x - iconSize / 2, target.position.y + iconSize * 0.45))
              val bounds = helper.localToScreen(Point(target.position.x + iconSize / 2, target.position.y - iconSize / 2))
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
                  keyboardAction("Escape", "Close dialog", close)
                },
                keyboard.addChild { _ =>
                  keyboardAction("Enter", "Close dialog", close)
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
