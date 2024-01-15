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
  override def run = {
    val root = dom.document.querySelector("#app")

    (for {
      startDragHub <- Hub.bounded[dom.MouseEvent](1)
      draggedHub <- Hub.bounded[dom.MouseEvent](1)
      dragInProgress <- Ref.make[Seq[Promise[Nothing, Unit]]](Seq.empty)

      elmt = div(
        svg(
          width := 800,
          height := 800,
          rect(
            width := "100%",
            height := "100%",
            fill := "#80a080"
          ),
          SVGOps.coordinateHelper { helper =>
            g(
              children <~~ startDragHub(_.mapZIO { downEvent =>
                for {
                  promise <- Promise.make[Nothing, Unit]
                  _ <- dragInProgress.update(_ :+ promise)
                } yield {
                  val startPos = helper.getClientPoint(downEvent)
                  val start = PathData.MoveTo(startPos.x, startPos.y)
                  dom.console.log("Appending from " + start)
                  children.Append(
                    path(
                      d <-- draggedHub(_
                        .mapZIO(event => promise.isDone.map((event, _)))
                        .takeWhile { (event, isDone) => !isDone }
                        .map { (event, _) =>
                          val pos = helper.getClientPoint(event)
                          PathData.LineTo(pos.x, pos.y)
                        }
                        .mapAccum(Seq[PathData](start)) { (seq, e) => (seq :+ e, seq :+ e) }
                        .map(PathData.render)
                      )
                    )
                  )
                }
              })
            )
          },
          onMouseDown --> startDragHub,
          onMouseMove(_.filter { e => (e.buttons & 1) != 0 }) --> draggedHub,
          onMouseUp(_.tap { _ =>
            for {
              promises <- dragInProgress.getAndSet(Seq.empty)
              _ <- ZIO.collectAll(promises.map(_.complete(ZIO.unit)))
            } yield ()
          }).run
        ),
      )
      _ <- elmt.mount(root)

      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
      _ = dom.console.log("EXITING")
    } yield ExitCode.success).catchAllCause { cause =>
      dom.console.log("Main failed")
      dom.console.log(cause)
      ZIO.succeed(ExitCode.failure)
    }
  }
}
