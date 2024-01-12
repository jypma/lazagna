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
import zio.lazagna.ZIOOps._
import zio.lazagna.dom.Element
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Events._
import zio.lazagna.dom.Attribute._
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.SVGOps

object StreamOps {
  def split[A, K, O](getKey: A => K)(makeOutput: (K, A, Hub[A]) => ZIO[Scope, Nothing, O]): ZPipeline[Scope, Nothing, Iterable[A], Seq[O]] = {
    ZPipeline.rechunk(1) >>> ZPipeline.fromPush {
      for {
        state <- Ref.make(LinkedHashMap.empty[K,(Hub[A],O)])
        push: (Option[Chunk[Iterable[A]]] => ZIO[Scope, Nothing, Chunk[Seq[O]]]) = _ match {
          // We're guaranteed one at a time because of rechunk(1) above
          // FIXME: We need to remove old outputs
          case Some(Chunk(elems)) =>
            for {
              i <- state.get
              toEmit <- ZIO.collectAll(elems.toSeq.map { elem =>
                val key = getKey(elem)
                i.get(key) match {
                  case Some((hub, out)) =>
                    hub.publish(elem).as(out)
                  case None =>
                    for {
                      hub <- Hub.bounded[A](1)
                      out <- makeOutput(key, elem, hub)
                    } yield {
                      i.put(key, (hub, out))
                      out
                    }
                }
              })
            } yield Chunk(toEmit)
          case _ => ZIO.succeed(Chunk.empty)
        }
      } yield push
    }
  }
}

object Draw extends ZIOAppDefault {

// import javascriptLogo from "/javascript.svg"
  @js.native @JSImport("/javascript.svg", JSImport.Default)
  val javascriptLogo: String = js.native

  val titles = ZStream.range(0, 10).zipLeft(ZStream.tick(1.second))
  val divs = (0 to 10).map(i => div(textContent := i.toString))
  val marker = div(textContent := "HERE!")

  override def run = {
    val root = dom.document.querySelector("#app")

    (for {
      hub    <- Hub.bounded[children.ChildOp](1)
      clickHub <- Hub.bounded[dom.MouseEvent](1)

      elmt = div(
        svg(
          width := 300,
          height := 300,
          rect(
            width := "100%",
            height := "100%",
            fill := "green"
          ),
          circle(
            cx := 150,
            cy := 150,
            r := 5,
            fill := "black"
          ),
          SVGOps.coordinateHelper { helper =>
            g(
              children <~~ clickHub.map { event =>
                val pos = helper.getClientPoint(event)
                children.Append(
                  circle(
                    cx := pos.x,
                    cy := pos.y,
                    r := 5,
                    fill := "black"
                  ),
                )
              }
            )
          },
          onClick --> clickHub
        ),
      )
      _ <- elmt.mount(root)

      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since we our daemon fibers do the real work.
      _ = dom.console.log("EXITING")
    } yield ExitCode.success).catchAllCause { cause =>
      dom.console.log("Main failed")
      dom.console.log(cause)
      ZIO.succeed(ExitCode.failure)
    }
  }
}
