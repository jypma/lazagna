package draw.client

import java.util.UUID

import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}

import zio.lazagna.Consumeable.given
import zio.lazagna.dom.http.Request.{AsDynamicJSON, HEAD, POST, RequestError}
import zio.lazagna.dom.http.WebSocket
import zio.lazagna.eventstore.EventStore
import zio.lazagna.{Consumeable, Setup}
import zio.stream.{SubscriptionRef, ZStream}
import zio.{Hub, Ref, Scope, Semaphore, ZIO, ZLayer}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.{DrawEvent, ObjectMoved, ScribbleContinued}
import draw.data.{DrawingState, ObjectState}
import org.scalajs.dom

import scalajs.js.typedarray._
import scalajs.js
import DrawingClient._

trait DrawingClient {
  def login(user: String, password: String, drawingId: UUID): ZIO[Scope & DrawingClient.Store, ClientError | RequestError, Drawing]
}

object DrawingClient {
  type Store = EventStore[DrawEvent, dom.DOMException | dom.ErrorEvent]
  import Drawing._

  case class ClientError(message: String)

  case class Config(server: String, port: Int, tls: Boolean, path: String) {
    private def http = if (tls) "https" else "http"
    private def ws = if (tls) "wss" else "ws"
    def baseUrl = s"${http}://${server}:${port}/${path}"
    def baseWs = s"${ws}://${server}:${port}/${path}"
  }

  def merge(a: DrawEvent, b: DrawEvent) = (a.body, b.body) match {
    case (ObjectMoved(id1, _, _), ObjectMoved(id2, Some(_), _)) if id1 == id2 =>
      Some(b)
    case (ScribbleContinued(id1, points1, _), ScribbleContinued(id2, points2, _)) if id1 == id2 =>
      Some(b.copy(body = ScribbleContinued(id2, points1 ++ points2)))
    case _ => None
  }

  val configTest = ZLayer.succeed {
    Config(dom.window.location.hostname, dom.window.location.port.toInt, dom.window.location.protocol == "https:", "api")
  }

  val live = ZLayer.fromZIO {
    for {
      config <- ZIO.service[Config]
    } yield new DrawingClient {
      var lastCommandTime: Long = 0

      override def login(user: String, password: String, drawingId: UUID) = Setup.start(for {
        store <- ZIO.service[Store]
        drawViewport <- SubscriptionRef.make(Viewport())
        connStatus <- SubscriptionRef.make[ConnectionStatus](Connected)
        state <- Ref.make(DrawingState())
        stateSemaphore <- Semaphore.make(1)
        lastEventNr <- SubscriptionRef.make(0L)
        stateChanges <- Hub.bounded[ObjectState[_]](16)
        newObjects <- Hub.bounded[ObjectState[_]](16)
        latencyHub <- Hub.bounded[Long](1)
        eventFilter <- EventFilter.make { (e: DrawEvent) =>
          store.publish(e).catchAll { err => ZIO.succeed {
            dom.console.log("Error publishing " + e)
            dom.console.log(err)
          }}
        }(merge)
        // FIXME: We really need a little path DSL to prevent injection here.
        loginResp <- POST(AsDynamicJSON, s"${config.baseUrl}/users/${user}/login?password=${password}")
        token <- loginResp.token.asInstanceOf[String] match {
          case s if s != null && !js.isUndefined(s) => ZIO.succeed(s)
          case _ => ZIO.fail(ClientError("Could not get token"))
        }
        version <- HEAD(s"${config.baseUrl}/drawings/${drawingId}?token=${token}").map(_.header("ETag").map(_.drop(1).dropRight(1).toLong).getOrElse(0L))
        latestSeen <- store.latestSequenceNr
        latestInStore <- if (latestSeen > version) {
          dom.console.log(s"Resetting client event store, since we've seen event ${latestSeen} but server only has ${version}")
          store.reset.as(0L)
        } else ZIO.succeed(latestSeen)
        socket <- WebSocket.handle(s"${config.baseWs}/drawings/${drawingId}/socket?token=${token}&afterSequenceNr=${latestInStore}")({ msg =>
          msg match {
            case m if m.data.isInstanceOf[ArrayBuffer] =>
              // TODO: Catch parse errors and fail accordingly
              val event = DrawEvent.parseFrom(new Int8Array(m.data.asInstanceOf[ArrayBuffer]).toArray)
              if (event.sequenceNr <= latestInStore) {
                println(s"WARN: Event not later than requested sequencrNr $latestSeen: $event")
              }
              eventFilter.publish(event)
            case _ => ZIO.unit
          }
        }, onClose = connStatus.set(Drawing.Disconnected))
        _ <- store.events.mapZIO { event =>
          stateSemaphore.withPermit {
            val publishLatency = if (lastCommandTime != 0) {
              latencyHub.publish(System.currentTimeMillis - lastCommandTime) *> ZIO.succeed { lastCommandTime = 0 }
            } else ZIO.unit

            state.modify(_.update(event)).flatMap {
              case (objectStates, isNew) =>
                ZIO.when(isNew)(ZIO.collectAll(objectStates.map(newObjects.publish))) *>
                ZIO.collectAll(objectStates.map(stateChanges.publish))
            } *> lastEventNr.set(event.sequenceNr) *> publishLatency
          }
        }.runDrain.forkScoped
      } yield new Drawing {
        override def perform(command: DrawCommand): ZIO[Any, Nothing, Unit] = {
          lastCommandTime = System.currentTimeMillis
          socket.send(command.toByteArray).catchAll { err =>
            // TODO: Reconnect or reload here
            dom.console.log(err)
            ZIO.unit
           }
        }
        override def initialVersion = latestInStore
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
        override def latency: Consumeable[Long] = latencyHub
      }).mapError { err => ClientError(err.toString) }
    }
  }
}
