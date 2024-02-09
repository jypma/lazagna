package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

import zio.lazagna.Consumeable._
import zio.lazagna.dom.http.Request.{AsDynamicJSON, HEAD, POST, RequestError}
import zio.lazagna.dom.http.WebSocket
import zio.lazagna.eventstore.EventStore
import zio.stream.SubscriptionRef
import zio.{Scope, ZIO, ZLayer}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent
import org.scalajs.dom

import scalajs.js.typedarray._
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

  val configTest = ZLayer.succeed {
    Config(dom.window.location.hostname, dom.window.location.port.toInt, dom.window.location.protocol == "https:", "api")
  }

  val live = ZLayer.fromZIO {
    for {
      config <- ZIO.service[Config]
      store <- ZIO.service[EventStore[DrawEvent, dom.DOMException | dom.ErrorEvent]]
      drawViewport <- SubscriptionRef.make(Drawing.Viewport())
      connStatus <- SubscriptionRef.make[Drawing.ConnectionStatus](Drawing.Connected)
    } yield new DrawingClient {
      override def login(user: String, password: String, drawingName: String) = (for {
        // FIXME: We really need a little path DSL to prevent injection here.
        loginResp <- POST(AsDynamicJSON, s"${config.baseUrl}/users/${user}/login?password=${password}")
        token <- loginResp.token.asInstanceOf[String] match {
          case s if s != null && !js.isUndefined(s) => ZIO.succeed(s)
          case _ => ZIO.fail(ClientError("Could not get token"))
        }
        version <- HEAD(s"${config.baseUrl}/drawings/${drawingName}?token=${token}").map(_.header("ETag").map(_.drop(1).dropRight(1).toLong).getOrElse(0L))
        after <- store.latestSequenceNr
        _ <- if (after > version) {
          dom.console.log(s"Resetting client event store, since we've seen event ${after} but server only has ${version}")
          store.reset
        } else ZIO.unit
        socket <- WebSocket.handle(s"${config.baseWs}/drawings/${drawingName}/socket?token=${token}&afterSequenceNr=${after}")({ msg =>
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
        }, onClose = connStatus.set(Drawing.Disconnected))
      } yield new Drawing {
        override def perform(command: DrawCommand): ZIO[Any, Nothing, Unit] = {
          socket.send(command.toByteArray).catchAll { err =>
            // TODO: Reconnect or reload here
            dom.console.log(err)
            ZIO.unit
           }
        }
        override def events  = store.events
        override def eventsAfter(lastSeenSequenceNr: Long) = store.eventsAfter(lastSeenSequenceNr)
        override def initialVersion = version
        override def viewport = drawViewport
        override def connectionStatus = connStatus
      }).mapError { err => ClientError(err.toString) }
    }
  }
}
