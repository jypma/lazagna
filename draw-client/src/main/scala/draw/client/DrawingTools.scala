package draw.client

import zio.lazagna.dom.Modifier
import zio.lazagna.dom.Modifier._
import zio.Ref
import zio.lazagna.dom.svg.SVGOps
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Attribute._
import zio.lazagna.Consumeable._

import draw.data.drawcommand.{ContinueScribble, DeleteScribble, DrawCommand, StartScribble}
import draw.data.point.Point

import org.scalajs.dom
import zio.Clock
import zio.Random
import zio.UIO
import java.util.Base64
import zio.ZLayer
import zio.stream.SubscriptionRef
import zio.ZIO
import zio.lazagna.dom.Alternative

trait DrawingTools {
  def renderHandlers: Modifier
  def renderToolbox: Modifier
}

object DrawingTools {
  private case class Tool(name: String, hint: String, icon: String, render: Modifier)

  val live = ZLayer.fromZIO {
    for {
      drawing <- ZIO.service[Drawing]
      tools = Seq(
        Tool("pencil", "Add pencil strokes", "✏️", pencil(drawing)),
        Tool("pan", "Hand (move drawing)", "🫳", Modifier.empty),
        Tool("note", "Add note", "📃", Modifier.empty),
        Tool("eraser", "Eraser (delete items)", "🗑️", eraser(drawing))
      )
      selectedTool <- SubscriptionRef.make(tools(0))
    } yield new DrawingTools {
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

  def eraser(drawing: Drawing): Modifier = onMouseDown.merge(onMouseMove)
    .filter { e => (e.buttons & 1) != 0 }
    .map(_.target)
    .collect { case elem: dom.Element => elem }
    .map(_.parentNode)
    .collect {
      case parent:dom.Element if parent.id.startsWith("scribble") =>
        val id = parent.id.substring("scribble".length)
        DrawCommand(DeleteScribble(id))
      }
    .mapZIO(drawing.perform _)

  def pencil(drawing: Drawing): Modifier = Modifier.unwrap(for {
    currentScribbleId <- Ref.make[String]("")
  } yield Modifier.combine(
    SVGOps.coordinateHelper { helper =>
      onMouseDown
        .filter { e => (e.buttons & 1) != 0 }
        .mapZIO(ev => makeUUID.flatMap(id => currentScribbleId.set(id).as(id)).map { id =>
          val pos = helper.getClientPoint(ev)
          DrawCommand(StartScribble(id, Some(Point(pos.x, pos.y))))
        })
        .merge(
          onMouseMove
            .filter { e => (e.buttons & 1) != 0 }
            .mapZIO(ev => currentScribbleId.get.map { id =>
              val pos = helper.getClientPoint(ev)
              DrawCommand(ContinueScribble(id, Seq(Point(pos.x, pos.y))))
            })
        )
        .mapZIO(drawing.perform _)
    })
  )
}
