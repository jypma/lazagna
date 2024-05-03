package draw.client

import java.util.UUID

import scala.util.Try

import zio.lazagna.Setup
import zio.lazagna.dom.Element._
import zio.lazagna.location.Location
import zio.stream.ZStream
import zio.{Chunk, Exit, ExitCode, Fiber, Schedule, Scope, ZIO, ZIOAppDefault, ZLayer, durationInt}

import org.scalajs.dom

object Main extends ZIOAppDefault {
  val drawingId = Try(UUID.fromString(Location.fragment.parameters("id"))).getOrElse(
    UUID.fromString("0314aaab-684c-49c6-bd29-0921a3897ce5") // TODO: Drawing selector
  )

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

  override def run = {
    for {
      _ <- dump.fork
      _ <- Setup.start {
        ZIO.service[DrawingViewer].flatMap(_.render(drawingId)).provideSome[Scope & Setup](
          DrawingClient.live,
          DrawingViewer.live,
          DrawingClient.configTest
        )
      }
      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success
  }.catchAllCause { cause =>
    dom.console.log(cause.prettyPrint)
    ZIO.succeed(ExitCode.failure)
  }
}
