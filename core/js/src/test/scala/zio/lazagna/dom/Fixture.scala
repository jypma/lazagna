package zio.lazagna.dom

import zio.ZIO
import zio.UIO
import org.scalajs.dom
import zio.durationInt
import java.util.concurrent.TimeoutException
import zio.test.Live
import zio.Scope

object Fixture {
  def makeDiv: ZIO[Scope, Nothing, dom.Element] = makeDiv("")

  /** Creates a DIV tag that is tied to a ZIO scope, deleting it when the scope ends. */
  def makeDiv(id: String = ""): ZIO[Scope, Nothing, dom.Element] = ZIO.acquireRelease(ZIO.succeed {
    val body = dom.document.querySelector("body")
    val div = dom.document.createElement("div")
    if (id != "") {
      div.setAttribute("id", id)
    }
    body.append(div);
    div
  }){ div => ZIO.succeed {
    div.parentNode.removeChild(div)
  }}

  def await(predicate: => Boolean): UIO[Unit] = awaitZIO(ZIO.succeed(predicate))

  def awaitZIO(predicate: UIO[Boolean]): UIO[Unit] = awaitZIO(predicate, 20)

  private def awaitZIO(predicate: UIO[Boolean], retries: Int): UIO[Unit] = {
    if (retries <= 0) ZIO.die(new TimeoutException("An await() call did not produce [true] in time.")) else {
      predicate.flatMap { done =>
        if (done) ZIO.unit else Live.live(awaitZIO(predicate, retries - 1).delay(100.milliseconds))
      }
    }
  }
}
