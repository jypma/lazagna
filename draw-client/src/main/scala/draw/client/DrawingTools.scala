package draw.client

import java.util.Base64

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Modifier._
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Modifier}
import zio.stream.SubscriptionRef
import zio.{Clock, Random, Ref, UIO, ZIO, ZLayer}

import draw.data.drawcommand.{ContinueScribble, DeleteScribble, DrawCommand, StartScribble}
import draw.data.point.Point
import org.scalajs.dom
import zio.lazagna.dom.Element
import zio.lazagna.dom.Children
import zio.Hub

trait DrawingTools {
  def renderKeyboard: Modifier
  def renderHandlers: Modifier
  def renderToolbox: Modifier
  def currentToolName: Consumeable[String]
}

object DrawingTools {
  private def keyboardAction(key: String, description: String, execute: UIO[Unit]): Element[dom.Element] = {
    val keyName = key match {
      case "Escape" => "Esc"
      case s => s.toUpperCase()
    }

    div(
      cls := "hint",
      div(
        cls := "key",
        textContent := keyName,
        onClick.mapZIO(_ => execute)
      ),
      div(cls := "description", textContent := description),
      windowEvents(
        onKeyDown.filter { e =>
          // The key is the one we're listening on:
          (e.key == key) &&
          // It's a function key, OR it's a normal key and we're not on an input element:
          (e.key.length() > 1 || !e.target.isInstanceOf[dom.HTMLInputElement])
        }.mapZIO{_ =>
          println(s"Running action for [${key}]")
          execute
        }
      )
    )
  }

  private case class Tool(name: String, hint: String, icon: String, render: Modifier)

  val live = ZLayer.scoped {
    for {
      drawing <- ZIO.service[Drawing]
      dialogs <- ZIO.service[Children]
      index <- ZIO.service[SymbolIndex]
      keyboard <- Children.make
      iconTool <- icon(drawing, dialogs, keyboard, index)
      tools = Seq(
        Tool("pencil", "Add pencil strokes", "✏️", pencil(drawing)),
        Tool("pan", "Hand (move drawing)", "🫳", hand(drawing, keyboard)),
        Tool("icon", "Add icon", "🚶", iconTool),
        Tool("eraser", "Eraser (delete items)", "🗑️", eraser(drawing))
      )
      selectedTool <- SubscriptionRef.make(tools(0))
    } yield new DrawingTools {
      override def currentToolName = selectedTool.map(_.name)

      override val renderHandlers = Alternative.mountOne(selectedTool)(_.render)

      override val renderToolbox = div(
        cls := "toolbox",
        tools.map { tool =>
          div(
            cls := "tool",
            input(`type` := "radio", name := "tool", id := s"tool-${tool.name}",
              checked <-- selectedTool.filter(_ == tool).as("true")),
            div(
              label (`for` := s"tool-${tool.name}", title := tool.hint, textContent := tool.icon,
                onClick.as(tool) --> selectedTool)
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

  private def hand(drawing: Drawing, keyboard: Children) = Modifier.unwrap(for {
    dragStart <- Ref.make(Point(0,0))
  } yield {
    SVGHelper { helper =>
      Modifier.combine(
        onMouseDown.mapZIO { event =>
          val pos = helper.getClientPoint(event)
          dragStart.set(Point(pos.x, pos.y))
        },
        onMouseMove
          .filter { e => (e.buttons & 1) != 0 }
          .mapZIO { event =>
            val pos = helper.getClientPoint(event)
            for {
              start <- dragStart.get
              _ <- drawing.viewport.update(_.pan(start.x - pos.x, start.y - pos.y))
            } yield ()
          },
        onWheel
          .mapZIO { event =>
            val factor = event.deltaMode match {
              case 0 => (event.deltaY * 0.01) + 1
              case 1 => (event.deltaY * 0.01) + 1
              case _ => (event.deltaY * 0.01) + 1
            }
            val pos = helper.getClientPoint(event)
            drawing.viewport.update(_.zoom(factor, pos.x, pos.y))
          },
        keyboard.child { _ =>
          div(
            keyboardAction("f", "Fit into view",
              drawing.viewport.update { _ .fit(helper.svg.getBBox()) }
            ),
            keyboardAction("e", "Export as SVG",
              ZIO.succeed { helper.triggerSVGDownload() }
            )
          )
        }
      )
    }
  })

  def eraser(drawing: Drawing): Modifier = onMouseDown.merge(onMouseMove)
    .filter { e => (e.buttons & 1) != 0 }
    .map(_.target)
    .collect { case elem: dom.Element =>
      dom.console.log(elem)
      elem }
    .map(_.parentNode)
    .collect {
      case parent:dom.Element if parent.id.startsWith("scribble") =>
        val id = parent.id.substring("scribble".length)
        DrawCommand(DeleteScribble(id))
      }
    .mapZIO(drawing.perform _)

  def pencil(drawing: Drawing): Modifier = Modifier.unwrap(for {
    currentScribbleId <- Ref.make[Option[String]](None)
  } yield SVGHelper { helper =>
    // FIXME: Don't emit an event until we've at least moved the mouse once (so there's something to draw and delete)
    onMouseDown
      .filter { e => (e.buttons & 1) != 0 }
      .mapZIO(ev => makeUUID.flatMap(id => currentScribbleId.set(Some(id)).as(id)).map { id =>
        val pos = helper.getClientPoint(ev)
        DrawCommand(StartScribble(id, Some(Point(pos.x, pos.y))))
      })
      .merge(
        onMouseMove
          .filter { e => (e.buttons & 1) != 0 }
          .mapZIO(ev => currentScribbleId.get.map((_, ev)))
          .collect { case (Some(id), ev) => (id, ev) }
          .map { (id, ev) =>
            val pos = helper.getClientPoint(ev)
            DrawCommand(ContinueScribble(id, Seq(Point(pos.x, pos.y))))
          }
      ).merge(
        onMouseUp.mapZIO(ev => currentScribbleId.set(None)).drain
      )
      .mapZIO(drawing.perform _)
  })

  private val iconSize = 64

  private def icon(drawing: Drawing, dialogs: Children, keyboard: Children, index: SymbolIndex) = for {
    searchResult <- Hub.bounded[SymbolIndex.Result](1)
    selectedIcon <- SubscriptionRef.make(SymbolRef.person)
    cursorPos <- SubscriptionRef.make[Option[dom.SVGPoint]](None)
  } yield {
    val selectDialog = dialogs.prepareChild { close =>
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
                  onClick.merge(onKeyDown.filter(_.key == "Enter")).mapZIO { _ =>
                    selectedIcon.set(symbol) *> close
                  }
                )
              }
            }
          ),
          div(
            input(typ := "text", placeholder := "Search icon...", list := "icon-dialog-list", focusNow, onInput.asTargetValue.mapZIO { text =>
              index.lookup(text).flatMap(searchResult.publish)
            }),
            datalist(id := "icon-dialog-list",
              Alternative.mountOne(searchResult) { _.completions.map { s => option(value := s) } }
            )
          ),
        ),
        keyboard.child { _ =>
          keyboardAction("Escape", "Close dialog", close)
        }
      )
    }

    SVGHelper { helper =>
      Modifier.combine(
        selectDialog,
        onMouseMove.mapZIO { e =>
          cursorPos.set(Some(helper.getClientPoint(e)))
        },
        keyboard.child { _ =>
          keyboardAction("t", "Select icon", selectDialog.create)
        },
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

}
