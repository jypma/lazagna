package draw.server.drawing

import java.nio.ByteBuffer
import java.time.Instant
import java.util.UUID

import zio.stream.ZStream
import zio.{Clock, Hub, IO, Ref, Scope, Semaphore, ZIO, ZLayer}

import draw.data.DrawingState
import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent
import palanga.zio.cassandra.ZStatement.StringOps
import palanga.zio.cassandra.{CassandraException, ZCqlSession}

object CassandraDrawings {
  import Drawings.DrawingError

  val make = for {
    mainScope <- ZIO.scope
    session <- ZIO.service[ZCqlSession]
    layer = ZLayer.succeed(session)
    initialDrawings <- {
      def firstDrawing: IO[DrawingError, Option[UUID]] = ZCqlSession.executeHeadOption(
        "SELECT drawingId FROM drawingEvents LIMIT 1"
          .toStatement
          .decodeAttempt(_.getUuid("drawingId"))
      ).mapError(toError).provide(layer)

      def nextDrawing(after: UUID): IO[DrawingError, Option[UUID]] = ZCqlSession.executeHeadOption(
        "SELECT drawingId FROM drawingEvents WHERE drawingId > ? AND sequenceNr = 0 LIMIT 1 ALLOW FILTERING"
          .toStatement
          .bind(after)
          .decodeAttempt(_.getUuid("drawingId"))
      ).mapError(toError).provide(layer)

      def drawingsAfter(after: UUID): IO[DrawingError, Seq[UUID]] =
        nextDrawing(after).flatMap {
          case None => ZIO.succeed(Seq.empty)
          case Some(id) => drawingsAfter(id).map(id +: _)
        }

      firstDrawing.flatMap {
          case None => ZIO.succeed(Seq.empty)
          case Some(id) => drawingsAfter(id).map(id +: _)
        }
    }
    knownDrawings <- Ref.make[Set[UUID]](initialDrawings.toSet)
    activeDrawings <- Ref.Synchronized.make[Map[UUID, (Scope, Drawing)]](Map.empty)
  } yield new Drawings {
    def list = ZStream.unwrap(knownDrawings.get.map(ZStream.fromIterable(_).map { id =>
      Drawings.DrawingRef(id, id.toString)
    }))

    def getDrawing(id: UUID) = activeDrawings.updateSomeAndGetZIO {
      case m if !m.contains(id) => for {
        subScope <- mainScope.fork
        drawing <- makeDrawing(id)
        _ <- AutoLayouter.make(drawing).provide(ZLayer.succeed(subScope))
      } yield m + (id -> (subScope, drawing))
    }.map(_(id)._2)

    private def makeDrawing(id: UUID): IO[DrawingError, Drawing] = for {
      startState <- currentDrawingEventsAfter(id, 0).runFold(DrawingState())((s,e) => s.update(e)._2)
      state <- Ref.make(startState)
      semaphore <- Semaphore.make(1)
      hub <- Hub.unbounded[DrawEvent]
      emit = (event: DrawEvent) => storeEvent(id, event) *> state.update(_.update(event)._2)
      _ <- if (startState.exists) ZIO.unit else for {
        events <- Clock.instant.map(startState.handleCreate)
        _ <- ZIO.collectAll(events.map(emit(_)))
      } yield ()
      _ <- knownDrawings.update(_ + id)
    } yield new Drawing {
      override def getState = state.get

      override def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit] = {
        semaphore.withPermit {
          for {
            now <- Clock.instant
            events <- state.get.map(_.handle(now, command))
            _ <- ZIO.collectAll(events.map { event =>
              emit(event) *> hub.publish(event)
            })
          } yield ()
        }
      }

      override def events = eventsAfter(0L)

      override def eventsAfter(sequenceNr: Long) = ZStream.unwrapScoped {
        semaphore.withPermit {
          ZStream.fromHubScoped(hub).map { liveEvents =>
            currentDrawingEventsAfter(id, sequenceNr) ++ liveEvents
          }
        }
      }

      override def version = state.get.map(_.lastSequenceNr)
    }

    def storeEvent(id: UUID, event: DrawEvent): ZIO[Any, DrawingError, Unit] = {
      val byteBuffer = ByteBuffer.wrap(event.toByteArray)
      ZCqlSession.untyped.execute(
        "INSERT INTO drawingEvents (drawingId, sequenceNr, event) VALUES (?,?,?) IF NOT EXISTS"
          .toStatement
          .bind(id, event.sequenceNr, byteBuffer)
      )
        .mapError(toError)
        .provide(layer)
        .unit
    }

    def currentDrawingEventsAfter(id: UUID, sequenceNr: Long) = {
      ZCqlSession.stream(
        "SELECT event FROM drawingEvents WHERE drawingId = ? AND sequenceNr > ? ORDER BY sequenceNr"
          .toStatement
          .bind(id, sequenceNr)
          .decodeAttempt { row =>
            val buf = row.getByteBuffer("event")
            val bytes = new Array[Byte](buf.remaining())
            buf.get(bytes)
            DrawEvent.parseFrom(bytes)
          }
      )
        .flattenChunks
        .mapError(toError)
        .provideLayer(layer)
    }
  }

  private def toError(x: CassandraException) = DrawingError(x.toString())
}
