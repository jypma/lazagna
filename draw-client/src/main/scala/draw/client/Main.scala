package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

import zio.lazagna.Setup
import zio.lazagna.dom.indexeddb.Schema.CreateObjectStore
import zio.lazagna.dom.indexeddb.{IndexedDB, Schema, ValueCodec}
import zio.lazagna.dom.weblocks.Lock
import zio.lazagna.eventstore.{CachedEventStore, EventStore, IndexedDBEventStore, PrunedEventStore}
import zio.stream.SubscriptionRef
import zio.{ExitCode, Scope, ZIO, ZIOAppDefault, ZLayer}

import draw.data.drawevent.DrawEvent
import org.scalajs.dom

import scalajs.js.typedarray._

object Main extends ZIOAppDefault {

  def main = for {
    renderer <- ZIO.service[DrawingRenderer]
    drawing <- ZIO.service[Drawing]
    tools <- ZIO.service[DrawingTools]
    _ <- renderer.render.mount(dom.document.querySelector("#app"))
    _ <- tools.renderToolbox.mount(dom.document.querySelector("#toolboxApp"))
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

  override def run = {
    for {
      writeLock <- ZLayer.fromZIO(Lock.makeAndLockExclusively(s"${drawingName}-writeLock")).memoize
      _ <- (for {
        client <- ZIO.service[DrawingClient]
        drawing <- client.login("jan", "jan", drawingName)
        _ <- main.provideSome[Scope](ZLayer.succeed(drawing), DrawingTools.live, DrawingRenderer.live)
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
