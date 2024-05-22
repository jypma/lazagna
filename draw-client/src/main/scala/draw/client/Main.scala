package draw.client

import zio.Cause.Fail
import zio.lazagna.Setup
import zio.lazagna.dom.Element._
import zio.stream.ZStream
import zio.{Cause, Chunk, Exit, ExitCode, Fiber, Schedule, Scope, ZIO, ZIOAppDefault, ZLayer, durationInt}

import org.scalajs.dom

object Main extends ZIOAppDefault {
  var knownDone = Set.empty[Fiber.Runtime[_,_]]
  def logFibers(fibers: Chunk[Fiber.Runtime[_,_]]): ZIO[Any,Nothing,String] = for {
    fibersWithStatus <- ZIO.collectAll(fibers.map{f => f.status.map((f, _))})
    newlyDone = fibersWithStatus.filter(_._2 == Fiber.Status.Done).map(_._1).toSet -- knownDone
    runningCount = fibersWithStatus.count(_._2 != Fiber.Status.Done)
    newlyWithResult <- ZIO.collectAll(newlyDone.map{f => f.poll.map((f, _))})
    _ <- ZIO.collectAll(newlyWithResult.map {
      case (fiber, Some(Exit.Failure(cause))) if !cause.isInterrupted =>
        println(s"Fiber failed with ${cause}")
        fiber.dump.flatMap(_.prettyPrint).map(println)
      case _ =>
        ZIO.unit
    })
  } yield {
    knownDone = knownDone ++ newlyDone
    s"Now ${runningCount} fibers."
  }

  val dump = ZStream.repeatZIOWithSchedule(Fiber.roots, Schedule.spaced(1.second)).mapZIO(logFibers).changes.debug.runDrain

  def handleError[T](cause: Cause[T]) = cause match {
    case Fail(DrawingClient.LoginNeeded(links), _) =>
      ZIO.serviceWithZIO[LoginHandler](_.render(links))
    case _ =>
      dom.console.log(cause.prettyPrint)
      ZIO.unit
  }

  def run(effect: ZIO[DrawingViewer & DrawingList & LoginHandler & Setup & Scope, Any, Any]) = Setup.start {
    effect.catchAllCause(handleError).provideSome[Scope & Setup](
      DrawingClient.live,
      DrawingClient.configTest,
      DrawingList.live,
      DrawingViewer.live,
      LoginHandler.live
    )
  }

  val routes = {
    import zio.lazagna.dom.Router._
    router(
      route(/("oauth2") / "authorize" && param("code") && param("state")) { (code, state) =>
        run(ZIO.serviceWithZIO[LoginHandler](_.handleCode(code, state)))
      },
      route(param("id").asUUID)(id => run(ZIO.service[DrawingViewer].flatMap(_.render(id)))),
      route(all)(_ => run(ZIO.service[DrawingList].flatMap(_.render)))
    )
  }

  override def run = {
    for {
      _ <- dump.fork
      _ <- routes.mount(dom.document.documentElement)
      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success
  }.catchAllCause { cause =>
    dom.console.log(cause.prettyPrint)
    ZIO.succeed(ExitCode.failure)
  }
}
