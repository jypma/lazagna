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
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Attribute._

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

/*
object MyElement {
  type Render = ZIO[Scope, Nothing, Unit]

  def render(element: Modifier, parent: dom.Element): Render = {
    dom.console.log("Starting render of " + element + " to " + parent)

    element.mount(parent)
  }

  def render(element: Modifier, id: String): Render = {
    render(element, dom.document.getElementById(id))
  }

  def div(children: Modifier*) = {
    val res = dom.document.createElement("div")
    MyElement(res, children.toSeq)
  }
}
 */


object Draw extends ZIOAppDefault {

// import javascriptLogo from "/javascript.svg"
  @js.native @JSImport("/javascript.svg", JSImport.Default)
  val javascriptLogo: String = js.native

  val titles = ZStream.range(0, 10).zipLeft(ZStream.tick(1.second))
  val divs = (0 to 10).map(i => div(textContent := i.toString))
  val marker = div(textContent := "HERE!")

  override def run = {
    val root = dom.document.querySelector("#app")
    dom.console.log(root)
    
    (for {
      _      <- Console.printLine("Starting progress bar demo.")  // Outputs on browser console log.
      hub    <- Hub.bounded[children.ChildOp](1)
      /*
      fiber2 <- ZStream.fromHubScoped(hub).flatMap { _.mapZIO { i =>
        dom.console.log("Recv: " + i)
        ZIO.unit
      }.runDrain.forkScoped.unit }
       */

      elmt = div(
        children <-- hub
      )
      _ <- elmt.mount(root)

      _ <- ZStream.fromIterable(marker +: divs).mapZIO { elem =>
        dom.console.log("Publishing " + elem)
        hub.publish(children.Append(elem))
      }.runDrain

      fiber1 <- titles
      .mapZIO { i =>
        dom.console.log("Send: " + i)
        hub.publish(children.InsertOrMove(marker, divs(i)))
      }.runDrain.forkScoped


      /*
      target <- ZIO.succeed(dom.document.createElement("pre"))
      _      <- ZIO.succeed {
        dom.console.log("appending")
        root.appendChild(target)
      } // "node" is provided in this page by mdoc.
      _      <- Console.printLine("Created " + target)
       _      <- update(target).repeat(Schedule.spaced(1.seconds))
       */


      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since we our daemon fibers do the real work.
      _ = dom.console.log("EXITING")
    } yield ExitCode.success).catchAllCause { cause =>
      dom.console.log("Main failed")
      dom.console.log(cause)
      ZIO.succeed(ExitCode.failure)
    }
  }

  def update(target: dom.Element) = {
    for {
      time   <- currentTime(TimeUnit.SECONDS)
      output <- ZIO.succeed(progress((time % 11).toInt, 10))
      _      <- ZIO.succeed(target.innerHTML = output)
    } yield ()
  }

  def progress(tick: Int, size: Int) = {
    val bar_length = tick
    val empty_length = size - tick
    val bar = "#" * bar_length + " " * empty_length
    s"$bar $bar_length%"
  }

  def Draw(): Unit = {
    dom.document.querySelector("#app").innerHTML = s"""
    <div>
      <a href="https://vitejs.dev" target="_blank">
        <img src="/vite.svg" class="logo" alt="Vite logo" />
      </a>
      <a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript" target="_blank">
        <img src="$javascriptLogo" class="logo vanilla" alt="JavaScript logo" />
      </a>
      <h1>Hello Scala.js and vite!</h1>
      <div class="card">
        <button id="counter" type="button"></button>
      </div>
      <p class="read-the-docs">
        Click on the Vite logo to learn more
      </p>
      <p>ZIO:</p>
      <div id="zioHolder"></div>
    </div>
  """

    setupCounter(dom.document.getElementById("counter"))

    dom.console.log("Starting")
    /*
     val effect = for {
     hub <- Hub.bounded[String](1)
     fiber1 <- titles.mapZIO(i => hub.publish(i.toString)).runDrain.fork
     elmt = MyElement.div(
     textContent <-- hub
     )
     fiber2 <- MyElement.render(elmt, "zioHolder")
     } yield ()
     */

    /*
    val effect = for {
      hub <- Hub.bounded[String](1)
      fiber1 <- titles.mapZIO { i =>
        dom.console.log("publish " + i)
        hub.publish(i.toString)
      }.runDrain.fork
      elmt = MyElement.div(
        textContent <-- hub
      )
      fiber2 <- MyElement.render(elmt, "zioHolder")
      _ <- fiber2.join
      _ <- fiber1.join
    } yield {
      dom.console.log("Done!")
    }

    Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.fork(effect)
     }
     */
  }

  def setupCounter(element: dom.Element): Unit = {
    var counter = 0

    def setCounter(count: Int): Unit = {
      counter = count
      element.innerHTML = s"count is $counter"
    }

    element.addEventListener("click", e => setCounter(counter + 1))
    setCounter(0)
  }
}
