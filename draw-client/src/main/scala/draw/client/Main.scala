package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}
import scalajs.js.typedarray._

import zio.{ExitCode, Scope, ZIO, ZIOAppDefault}

import org.scalajs.dom
import zio.ZLayer
import zio.lazagna.dom.indexeddb.IndexedDB
import zio.lazagna.dom.indexeddb.Schema
import zio.lazagna.dom.indexeddb.ValueCodec
import zio.lazagna.dom.indexeddb.Schema.CreateObjectStore
import draw.data.drawevent.DrawEvent

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

  val eventStore = ZLayer.fromZIO(EventStore.indexedDB[DrawEvent,ArrayBuffer](s"events", s"${drawingName}-events", _.sequenceNr).flatMap(EventStore.cached))

  val database = ZLayer.fromZIO(IndexedDB.open(s"drawing-${drawingName}", Schema(
    CreateObjectStore("events")
  )))

  override def run = ZIO.scoped {
    (for {
      client <- ZIO.service[DrawingClient]
      drawing <- client.login("jan", "jan", drawingName)
      _ <- main.provideSome[Scope](ZLayer.succeed(drawing), DrawingTools.live, DrawingRenderer.live)
      store <- ZIO.service[EventStore[DrawEvent, dom.DOMException | dom.ErrorEvent]]
      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success)
      .catchAllCause { cause =>
      dom.console.log(cause.prettyPrint)
      ZIO.succeed(ExitCode.failure)
    }.provideSome[Scope](DrawingClient.live, DrawingClient.configTest, database, eventStore)
  }
}
