package draw.client

import java.util.UUID

import zio.lazagna.Setup
import zio.lazagna.dom.Element._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.http.Request.RequestError
import zio.lazagna.dom.indexeddb.IndexedDB.Blocked
import zio.lazagna.dom.indexeddb.Schema.CreateObjectStore
import zio.lazagna.dom.indexeddb.{IndexedDB, Schema, ValueCodec}
import zio.lazagna.dom.weblocks.Lock
import zio.lazagna.dom.{Children, Modifier}
import zio.lazagna.eventstore.{CachedEventStore, EventStore, IndexedDBEventStore, PrunedEventStore}
import zio.stream.SubscriptionRef
import zio.{Scope, ZIO, ZLayer}

import draw.client.DrawingClient.ClientError
import draw.client.render.{DrawingRenderer, RenderState}
import draw.client.tools.DrawingTools
import draw.data.drawevent.DrawEvent
import org.scalajs.dom
import org.scalajs.dom.{DOMException, ErrorEvent}

import scalajs.js.typedarray.{ArrayBuffer, Int8Array}
import scalajs.js.typedarray._

trait DrawingViewer {
  def render(drawingId: UUID): ZIO[Scope & Setup, ClientError | RequestError | Blocked | ErrorEvent | DOMException, Unit]
}

object DrawingViewer {
  val live = ZLayer.fromZIO(for {
    client <- ZIO.service[DrawingClient]
    dialogs <- Children.make
  } yield new DrawingViewer {
    def render(drawingId: UUID) = {
      implicit val drawEventCodec: ValueCodec[DrawEvent, ArrayBuffer] = new ValueCodec[DrawEvent, ArrayBuffer] {
        override def encode(e: DrawEvent): ArrayBuffer = e.toByteArray.toTypedArray.buffer
        override def decode(b: ArrayBuffer): DrawEvent = DrawEvent.parseFrom(new Int8Array(b).toArray)
      }

      def printContents[Err](store: EventStore[DrawEvent,Err]) = for {
        last <- store.latestSequenceNr
        _ <- store.events.takeUntil(e => e.sequenceNr >= last).debug.runDrain.unless(last <= 0)
      } yield ()

      def debugView(drawing: Drawing): Modifier[_] = div(
        textContent <-- drawing.latency.sliding(10).map(c => s"Latency: ${(c.sum / 10)}ms")
      )

      val eventStore = ZLayer.fromZIO {
        for {
          lock <- ZIO.service[SubscriptionRef[Boolean]]
          _ = println("Creating store")
          store <- IndexedDBEventStore.make[DrawEvent,ArrayBuffer](s"events", lock, _.sequenceNr)
          prunedStore <- IndexedDBEventStore.make[DrawEvent,ArrayBuffer](s"events-pruned", lock, _.sequenceNr)
          pruned <- PrunedEventStore.make(store, prunedStore, lock, Pruned.State())(_.prune(_))(_.recover(_))
          _ = println("Printing store")
          _ <- printContents(pruned)
          cached <- CachedEventStore.make(pruned)
          _ = println("Done")
        } yield cached
      }

      val database = ZLayer.fromZIO(IndexedDB.open(s"drawing-${drawingId}", Schema(
        CreateObjectStore("events"),
        CreateObjectStore("events-pruned")
      )))

      println(s"Logging in for drawing ${drawingId}")
      (for {
        writeLock <- ZLayer.fromZIO(Lock.makeAndLockExclusively(s"${drawingId}-writeLock")).memoize
        drawing <- client.getDrawing(drawingId).provideSome[Scope & Setup](writeLock, database, eventStore)
        _ <- (for {
          renderer <- ZIO.service[DrawingRenderer]
          tools <- ZIO.service[DrawingTools]
          _ <- dialogs.render.mount(dom.document.querySelector("#dialogApp"))
          _ <- renderer.render.mount(dom.document.querySelector("#app"))
          _ <- tools.renderToolbox.mount(dom.document.querySelector("#toolboxApp"))
          _ <- tools.renderKeyboard.mount(dom.document.querySelector("#keyboardApp"))
          _ <- debugView(drawing).mount(dom.document.querySelector("#debugApp"))
        } yield ()).provideSome[Scope](
          DrawingRenderer.live,
          DrawingTools.live,
          RenderState.live,
          ZLayer.succeed(drawing),
          ZLayer.succeed(dialogs),
          ZLayer.fromZIO(SymbolIndex.make)
        )
      } yield ())
    }
  })
}
