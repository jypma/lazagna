package draw.client

import java.util.UUID

import scala.scalajs.js.JSON
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array}
import scala.util.Try

import zio.lazagna.Consumeable.given
import zio.lazagna.dom.http.Request.{AsDynamicJSON, GET, HEAD, POST, JSONAs, RequestError, RequestFailed, AsResponse}
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
  import DrawingClient._
  def user: ZIO[Any, ClientError, User]

  def getDrawing(drawingId: UUID): ZIO[Scope & DrawingClient.Store, ClientError, Drawing]
  def makeDrawing: ZIO[Any, ClientError, UUID]
  def list: ZIO[Any, ClientError, Seq[DrawingClient.DrawingRef]]
}

object DrawingClient {
  case class DrawingRef(id: UUID, name: String)
  case class User(id: UUID, nickname: String)

  type Store = EventStore[DrawEvent, dom.DOMException | dom.ErrorEvent]
  import Drawing._

  sealed trait ClientError
  case class ClientFailure(message: String) extends ClientError
  case class LoginNeeded(links: Seq[(String, String)]) extends ClientError

  private def toError(error: RequestError): ClientError = error match {
    case RequestFailed(401, msg) =>
      Try {
        println("parsing: " + msg)
        val body = JSON.parse(msg)
        val links = body.login.asInstanceOf[js.Array[js.Dynamic]]
        LoginNeeded(links.map { l => (l.name.asInstanceOf[String], l.link.asInstanceOf[String]) }.toSeq)
      }.getOrElse(ClientFailure(error.toString))
    case other => ClientFailure(other.toString)
  }

  case class Config(server: String, port: Int, tls: Boolean, path: String) {
    private def http = if (tls) "https" else "http"
    private def ws = if (tls) "wss" else "ws"
    def baseUrl = s"${http}://${server}:${port}${path}"
    def baseWs = s"${ws}://${server}:${port}${path}"
  }

  def merge(a: DrawEvent, b: DrawEvent) = (a.body, b.body) match {
    case (ObjectMoved(id1, _, _), ObjectMoved(id2, Some(_), _)) if id1 == id2 =>
      Some(b)
    case (ScribbleContinued(id1, points1, _), ScribbleContinued(id2, points2, _)) if id1 == id2 =>
      Some(b.copy(body = ScribbleContinued(id2, points1 ++ points2)))
    case _ => None
  }

  val configTest = ZLayer.succeed {
    Config(dom.window.location.hostname, dom.window.location.port.toInt, dom.window.location.protocol == "https:", "")
  }

  val live = ZLayer.fromZIO {
    for {
      config <- ZIO.service[Config]
    } yield new DrawingClient {
      override def user = for {
        resp <- GET(AsDynamicJSON, s"${config.baseUrl}/user").mapError(toError)
      } yield {
        User(UUID.fromString(resp.id.asInstanceOf[String]), resp.nickname.asInstanceOf[String])
      }

      var lastCommandTime: Long = 0

      override def list = for {
        res <- GET(JSONAs[js.Array[js.Dynamic]], s"${config.baseUrl}/drawings").mapError(toError)
      } yield res.toSeq.map { d =>
        DrawingRef(UUID.fromString(d.id.asInstanceOf[String]), d.name.asInstanceOf[String])
      }

      override def makeDrawing = for {
        res <- POST(AsResponse, s"${config.baseUrl}/drawings").mapError(toError)
        id <- ZIO.fromOption(res.header("Location").map(UUID.fromString))
          .mapError(_ => ClientFailure("Couldn't create drawing: " + res.header("Location")))
      } yield id

      override def getDrawing(drawingId: UUID) = Setup.start(for {
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
        // FIXME: We really need a little path DSL to prevent injection here. Perhaps zio-http JS?
        version <- HEAD(s"${config.baseUrl}/drawings/${drawingId}").map(_.header("ETag").map(_.drop(1).dropRight(1).toLong).getOrElse(0L))
        latestSeen <- store.latestSequenceNr
        latestInStore <- if (latestSeen > version) {
          dom.console.log(s"Resetting client event store, since we've seen event ${latestSeen} but server only has ${version}")
          store.reset.as(0L)
        } else ZIO.succeed(latestSeen)
        socket <- WebSocket.handle(s"${config.baseWs}/drawings/${drawingId}/socket?afterSequenceNr=${latestInStore}")({ msg =>
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
      }).mapError {
        case rq:RequestError => toError(rq)
        case other => ClientFailure(other.toString)
      }
    }
  }
}
