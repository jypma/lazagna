package draw.client

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

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
import zio.Ref
import zio.Hub
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.stream.ZStream
import zio.lazagna.Setup
import zio.Semaphore

trait DrawingClient {
  def login(user: String, password: String, drawingName: String): ZIO[Scope, ClientError | RequestError, Drawing]
}

object DrawingClient {
  import Drawing._

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
      drawViewport <- SubscriptionRef.make(Viewport())
      connStatus <- SubscriptionRef.make[ConnectionStatus](Connected)
      state <- Ref.make(DrawingState(Map.empty)) // TODO investigate Ref.Synchronized to close over state
      stateSemaphore <- Semaphore.make(1)
      lastEventNr <- SubscriptionRef.make(0L)
      stateChanges <- Hub.bounded[ObjectState[_]](16)
      newObjects <- Hub.bounded[ObjectState[_]](16)
    } yield new DrawingClient {
      override def login(user: String, password: String, drawingName: String) = Setup.start(for {
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
        _ <- store.events.mapZIO { event =>
          stateSemaphore.withPermit {
            state.modify(_.update(event)).flatMap {
              case (Some(objectState), isNew) =>
                newObjects.publish(objectState).when(isNew) *> stateChanges.publish(objectState)
              case _ =>
                ZIO.unit
            } *> lastEventNr.set(event.sequenceNr)
          }
        }.runDrain.forkScoped
      } yield new Drawing {
        override def perform(command: DrawCommand): ZIO[Any, Nothing, Unit] = {
          socket.send(command.toByteArray).catchAll { err =>
            // TODO: Reconnect or reload here
            dom.console.log(err)
            ZIO.unit
           }
        }
        override def initialVersion = version
        override def viewport = drawViewport
        override def connectionStatus = connStatus
        override def objectState(id: String): Consumeable[ObjectState[_]] = ZStream.unwrapScoped {
          stateSemaphore.withPermit {
            state.get
              .map(_.objects.get(id).toSeq)
              .map(ZStream.fromIterable(_) ++ stateChanges.filter(_.id == id))
          }
        }
        override def initialObjectStates: Consumeable[ObjectState[_]] = ZStream.unwrapScoped {
          stateSemaphore.withPermit {
            state.get.map(_.objects.values).flatMap { initial =>
              ZStream.fromHubScoped(newObjects).map { stream =>
                ZStream.fromIterable(initial) ++ stream
              }
            }
          }
        }
        override def currentVersion: Consumeable[Long] = lastEventNr
      }).mapError { err => ClientError(err.toString) }
    }
  }
}
