package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

import zio.lazagna.Setup
import zio.lazagna.dom.Children
import zio.lazagna.dom.indexeddb.Schema.CreateObjectStore
import zio.lazagna.dom.indexeddb.{IndexedDB, Schema, ValueCodec}
import zio.lazagna.dom.weblocks.Lock
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element._
import zio.lazagna.eventstore.{CachedEventStore, EventStore, IndexedDBEventStore, PrunedEventStore}
import zio.stream.{SubscriptionRef, ZStream}
import zio.{Chunk, Exit, ExitCode, Fiber, Schedule, Scope, ZIO, ZIOAppDefault, ZLayer, durationInt}

import draw.data.drawevent.DrawEvent
import org.scalajs.dom

import scalajs.js.typedarray._
import zio.lazagna.dom.Modifier

object Main extends ZIOAppDefault {

  def debugView(drawing: Drawing): Modifier = div(
    textContent <-- drawing.latency.sliding(10).map(c => s"Latency: ${(c.sum / 10)}ms")
  )

  def main = for {
    renderer <- ZIO.service[DrawingRenderer]
    drawing <- ZIO.service[Drawing]
    tools <- ZIO.service[DrawingTools]
    dialogs <- ZIO.service[Children]
    _ <- dialogs.render.mount(dom.document.querySelector("#dialogApp"))
    _ <- renderer.render.mount(dom.document.querySelector("#app"))
    _ <- tools.renderToolbox.mount(dom.document.querySelector("#toolboxApp"))
    _ <- tools.renderKeyboard.mount(dom.document.querySelector("#keyboardApp"))
    _ <- debugView(drawing).mount(dom.document.querySelector("#debugApp"))

  } yield ()

  implicit val drawEventCodec: ValueCodec[DrawEvent, ArrayBuffer] = new ValueCodec[DrawEvent, ArrayBuffer] {
    override def encode(e: DrawEvent): ArrayBuffer = e.toByteArray.toTypedArray.buffer
    override def decode(b: ArrayBuffer): DrawEvent = DrawEvent.parseFrom(new Int8Array(b).toArray)
  }

  val drawingName = "test"

  val eventStore = ZLayer.fromZIO {
    Setup.start {
      for {
        lock <- ZIO.service[SubscriptionRef[Boolean]]
        store <- IndexedDBEventStore.make[DrawEvent,ArrayBuffer](s"events", lock, _.sequenceNr)
        prunedStore <- IndexedDBEventStore.make[DrawEvent,ArrayBuffer](s"events-pruned", lock, _.sequenceNr)
        pruned <- PrunedEventStore.make(store, prunedStore, lock, Pruned.State())(_.prune(_))(_.recover(_))
        cached <- CachedEventStore.make(pruned)
      } yield cached
    }
  }

  val database = ZLayer.fromZIO(IndexedDB.open(s"drawing-${drawingName}", Schema(
    CreateObjectStore("events"),
    CreateObjectStore("events-pruned")
  )))

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
      writeLock <- ZLayer.fromZIO(Lock.makeAndLockExclusively(s"${drawingName}-writeLock")).memoize
      _ <- (for {
        client <- ZIO.service[DrawingClient]
        dialogs <- Children.make
        drawing <- client.login("jan", "jan", drawingName)
        _ <- main.provideSome[Scope](ZLayer.succeed(drawing), DrawingTools.live, DrawingRenderer.live, ZLayer.succeed(dialogs), ZLayer.fromZIO(SymbolIndex.make))
        store <- ZIO.service[EventStore[DrawEvent, dom.DOMException | dom.ErrorEvent]]
        _ = dom.console.log("Main is ready.")
        _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
      } yield ()).provideSome[Scope](
        DrawingClient.live, DrawingClient.configTest, database, eventStore, writeLock
      )
    } yield ExitCode.success
  }.catchAllCause { cause =>
    dom.console.log(cause.prettyPrint)
    ZIO.succeed(ExitCode.failure)
  }
}
