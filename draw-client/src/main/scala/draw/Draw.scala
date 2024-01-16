package draw

import scala.scalajs.js

import org.scalajs.dom
import zio.stream.ZStream
import zio._
import zio.Clock._
import zio.stream.ZPipeline
import org.scalajs.dom.Node
import zio.IO
import java.util.concurrent.TimeUnit
import scala.collection.mutable.LinkedHashMap
import scala.scalajs.js.annotation.JSImport
import zio.lazagna.dom.FastDiff
import zio.lazagna.dom.Element
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Attribute._
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.svg.SVGOps
import zio.lazagna.dom.svg.PathData
import zio.lazagna.dom.Modifier

object Draw extends ZIOAppDefault {
  override def run = {
    val root = dom.document.querySelector("#app")

    (for {
      commandHub <- Hub.bounded[DrawCommand](1)
      nextScribbleId <- Ref.make[Long](0)

      elmt = div(
        svg(
          width := 800,
          height := 800,
          rect(
            width := "100%",
            height := "100%",
            fill := "#80a080"
          ),
          DrawCommand.render(commandHub),
          SVGOps.coordinateHelper { helper =>
            Modifier.combine(
              onMouseDown(_
                .filter { e => (e.buttons & 1) != 0 }
                .filter(ev => !ev.getModifierState("Alt"))
                .mapZIO(ev => nextScribbleId.get.map { id =>
                  val pos = helper.getClientPoint(ev)
                  DrawCommand.StartScribble(id, DrawCommand.Point(pos.x, pos.y))
                })
              ) --> commandHub,
              onMouseDown.merge(onMouseMove)(_
                .filter { e => (e.buttons & 1) != 0 }
                .filter { ev => ev.getModifierState("Alt") }
                .map(_.target)
                .collect { case elem: dom.Element => elem }
                .map(_.parentNode)
                .collect {
                  case parent:dom.Element if parent.id.startsWith("scribble") =>
                    val id = parent.id.substring("scribble".length).toLong
                    DrawCommand.DeleteScribble(id)
                }
              ) --> commandHub,
              onMouseMove(_
                .filter { e => (e.buttons & 1) != 0 }
                .mapZIO(ev => nextScribbleId.get.map(id =>
                  val pos = helper.getClientPoint(ev)
                  DrawCommand.ContinueScribble(id, Seq(DrawCommand.Point(pos.x, pos.y)))
                ))
              ) --> commandHub,
              onMouseUp(_
                .mapZIO(_ => nextScribbleId.update(_ + 1))
              ).run
            )
          }
        )
      )
      _ <- elmt.mount(root)

      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
      _ = dom.console.log("EXITING")
    } yield ExitCode.success).catchAllCause { cause =>
      dom.console.log("Main failed")
      dom.console.log(cause.prettyPrint)
      ZIO.succeed(ExitCode.failure)
    }
  }
}
