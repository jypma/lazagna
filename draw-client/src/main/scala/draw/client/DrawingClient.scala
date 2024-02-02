package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}
import scalajs.js.typedarray._

import zio.lazagna.dom.http.Request.{AsDynamicJSON, POST, HEAD, RequestError}
import zio.lazagna.dom.http.WebSocket
import zio.{Scope, ZIO, ZLayer}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent
import org.scalajs.dom

import scalajs.js
import DrawingClient._
import zio.stream.SubscriptionRef
import zio.stream.ZStream
import zio.lazagna.dom.indexeddb.IndexedDB
import zio.lazagna.dom.indexeddb.Database
import zio.lazagna.dom.indexeddb.Schema
import zio.lazagna.dom.indexeddb.Schema.CreateObjectStore

trait DrawingClient {
  def login(user: String, password: String, drawingName: String): ZIO[Scope, ClientError | RequestError, Drawing]
}

object DrawingClient {
  case class ClientError(message: String)

  case class Config(server: String, port: Int, tls: Boolean, path: String) {
    private def http = if (tls) "https" else "http"
    private def ws = if (tls) "wss" else "ws"
    def baseUrl = s"${http}://${server}:${port}/${path}"
    def baseWs = s"${ws}://${server}:${port}/${path}"
  }

  val configTest = ZLayer.succeed(
    Config(dom.window.location.hostname, dom.window.location.port.toInt, dom.window.location.protocol == "https", "api")
  )

  implicit val drawEventCodec: EventStore.Codec[DrawEvent, ArrayBuffer] = new EventStore.Codec[DrawEvent, ArrayBuffer] {
    override def encode(e: DrawEvent): ArrayBuffer = e.toByteArray.toTypedArray.buffer
    override def decode(b: ArrayBuffer): DrawEvent = DrawEvent.parseFrom(new Int8Array(b).toArray)
  }

  val live = ZLayer.fromZIO {
    for {
      config <- ZIO.service[Config]
      drawViewport <- SubscriptionRef.make(Drawing.Viewport())
    } yield new DrawingClient {
      override def login(user: String, password: String, drawingName: String) = (for {
        database <- IndexedDB.open(s"drawing-${drawingName}", Schema(
          CreateObjectStore("events")
        ))
        // FIXME: We really need a little path DSL to prevent injection here.
        loginResp <- POST(AsDynamicJSON, s"${config.baseUrl}/users/${user}/login?password=${password}")
        token <- loginResp.token.asInstanceOf[String] match {
          case s if s != null && !js.isUndefined(s) => ZIO.succeed(s)
          case _ => ZIO.fail(ClientError("Could not get token"))
        }
        version <- HEAD(s"${config.baseUrl}/drawings/${drawingName}?token=${token}").map(_.header("ETag").map(_.drop(1).dropRight(1).toLong).getOrElse(0L))
        store <- EventStore.indexedDB[DrawEvent,ArrayBuffer](s"events", _.sequenceNr).provideSome[Scope](ZLayer.succeed(database))
        after <- store.latestSequenceNr
        socket <- WebSocket.handle(s"${config.baseWs}/drawings/${drawingName}/socket?token=${token}&afterSequenceNr=${after}") { msg =>
          msg match {
            case m if m.data.isInstanceOf[ArrayBuffer] =>
              // TODO: Catch parse errors and fail accordingly
              val event = DrawEvent.parseFrom(new Int8Array(m.data.asInstanceOf[ArrayBuffer]).toArray)
              store.publish(event).catchAll { err => ZIO.succeed {
                dom.console.log("Error publishing event:")
                dom.console.log(err)
              }}
            case _ => ZIO.unit
          }
        }
      } yield new Drawing {
        override def perform(command: DrawCommand): ZIO[Any, Nothing, Unit] = {
          socket.send(command.toByteArray).catchAll { err =>
          // FIXME: Show error or reconnect
            ZIO.unit
           }
        }
        override def events  = store.events.catchAll { err =>
          dom.console.log("Error reading event:")
          dom.console.log(err)
          ZStream.empty
        }
        override def eventsAfter(lastSeenSequenceNr: Long) = store.eventsAfter(lastSeenSequenceNr).catchAll { err =>
          dom.console.log("Error reading event:")
          dom.console.log(err)
          ZStream.empty
        }
        override def initialVersion = version
        override def viewport = drawViewport
      }).mapError { err => ClientError(err.toString) }
    }
  }
}
