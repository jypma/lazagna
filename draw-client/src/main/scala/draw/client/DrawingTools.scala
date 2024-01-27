package draw.client

import zio.lazagna.dom.Modifier
import zio.Ref
import zio.lazagna.dom.svg.SVGOps
import zio.lazagna.dom.Events._

import draw.data.drawcommand.{ContinueScribble, DeleteScribble, DrawCommand, StartScribble}
import draw.data.point.Point

import org.scalajs.dom
import zio.Clock
import zio.Random
import zio.UIO
import java.util.Base64

object DrawingTools {
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

  def pencil(drawing: Drawing): Modifier = Modifier.unwrap(for {
    currentScribbleId <- Ref.make[String]("")
  } yield Modifier.combine(
    SVGOps.coordinateHelper { helper =>
      onMouseDown
        .filter { e => (e.buttons & 1) != 0 }
        .filter(ev => !ev.getModifierState("Alt"))
        .mapZIO(ev => makeUUID.flatMap(id => currentScribbleId.set(id).as(id)).map { id =>
          val pos = helper.getClientPoint(ev)
          DrawCommand(StartScribble(id, Some(Point(pos.x, pos.y))))
        })
        .merge(
          onMouseDown
            .merge(onMouseMove)
            .filter { e => (e.buttons & 1) != 0 }
            .filter { ev => ev.getModifierState("Alt") }
            .map(_.target)
            .collect { case elem: dom.Element => elem }
            .map(_.parentNode)
            .collect {
              case parent:dom.Element if parent.id.startsWith("scribble") =>
                val id = parent.id.substring("scribble".length)
                DrawCommand(DeleteScribble(id))
            }
        )
        .merge(
          onMouseMove
            .filter { e => (e.buttons & 1) != 0 }
            .filter(ev => !ev.getModifierState("Alt"))
            .mapZIO(ev => currentScribbleId.get.map { id =>
              val pos = helper.getClientPoint(ev)
              DrawCommand(ContinueScribble(id, Seq(Point(pos.x, pos.y))))
            })
        )
        .mapZIO(drawing.perform _)
    })
  )
}
