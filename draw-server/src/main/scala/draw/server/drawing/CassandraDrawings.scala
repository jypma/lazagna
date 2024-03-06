package draw.server.drawing

import java.nio.ByteBuffer
import java.time.Instant
import java.util.UUID

import zio.stream.ZStream
import zio.{Clock, Hub, IO, Ref, Semaphore, ZIO, ZLayer}

import draw.data.DrawingState
import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent
import palanga.zio.cassandra.ZStatement.StringOps
import palanga.zio.cassandra.{CassandraException, ZCqlSession}

object CassandraDrawings {
  import Drawings.DrawingError

  val make = for {
    session <- ZIO.service[ZCqlSession]
  } yield new Drawings {
    val layer = ZLayer.succeed(session)

    def getDrawing(id: UUID): IO[DrawingError, Drawing] = for {
      startState <- currentDrawingEventsAfter(id, 0).runFold(DrawingState())((s,e) => s.update(e)._2)
      state <- Ref.make(startState)
      semaphore <- Semaphore.make(1)
      hub <- Hub.unbounded[DrawEvent]
      emit = (event: DrawEvent) => storeEvent(id, event) *> state.update(_.update(event)._2)
      _ <- if (startState.exists) ZIO.unit else for {
        events <- Clock.instant.map(startState.handleCreate)
        _ <- ZIO.collectAll(events.map(emit(_)))
      } yield ()
    } yield new Drawing {
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
