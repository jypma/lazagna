package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

import zio.lazagna.dom.http.Request.{AsDynamicJSON, POST, RequestError}
import zio.lazagna.dom.http.WebSocket
import zio.{Scope, ZIO, ZLayer}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent
import org.scalajs.dom

import scalajs.js
import DrawingClient._

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

  val live = ZLayer.fromZIO {
    for {
      config <- ZIO.service[Config]
    } yield new DrawingClient {
      override def login(user: String, password: String, drawingName: String) = (for {
        // FIXME: We really need a little path DSL to prevent injection here.
        loginResp <- POST(AsDynamicJSON, s"${config.baseUrl}/users/${user}/login?password=${password}")
        token <- loginResp.token.asInstanceOf[String] match {
          case s if s != null && !js.isUndefined(s) => ZIO.succeed(s)
          case _ => ZIO.fail(ClientError("Could not get token"))
        }
        store <- EventStore.make[DrawEvent](_.sequenceNr)
        socket <- WebSocket.handle(s"${config.baseWs}/drawings/${drawingName}/socket?token=${token}") { msg =>
          msg match {
            case m if m.data.isInstanceOf[ArrayBuffer] =>
              // TODO: Catch parse errors and fail accordingly
              val event = DrawEvent.parseFrom(new Int8Array(m.data.asInstanceOf[ArrayBuffer]).toArray)
              store.publish(event)
            case _ => ZIO.unit
          }
        }
      } yield new Drawing {
        override def perform(command: DrawCommand): ZIO[Any, Nothing, Unit] = socket.send(command.toByteArray).catchAll { err =>
          // FIXME: Show error or reconnect
          ZIO.unit
        }
        override def events  = store.events
        override def eventsAfter(lastSeenSequenceNr: Long) = store.eventsAfter(lastSeenSequenceNr)
      })
    }
  }
}
