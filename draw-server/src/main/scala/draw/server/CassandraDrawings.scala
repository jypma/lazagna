package draw.server

import zio.{ZIO, IO}
import draw.server.Drawings.DrawingError
import zio.stream.ZStream
import draw.data.drawevent.DrawEvent
import palanga.zio.cassandra.ZCqlSession
import palanga.zio.cassandra.ZStatement.StringOps
import palanga.zio.cassandra.CassandraException
import zio.ZLayer
import java.util.UUID
import zio.Clock
import draw.data.drawcommand.{ContinueScribble, CreateIcon, DeleteObject, DrawCommand, MoveObject, StartScribble, LabelObject}
import draw.data.drawevent.{DrawingCreated, IconCreated, ObjectDeleted, ObjectMoved, ScribbleContinued, ScribbleStarted, ObjectLabelled}
import draw.data.point.Point
import zio.Ref
import java.time.Instant
import draw.data.drawevent.DrawEventBody
import java.nio.ByteBuffer
import zio.Semaphore
import zio.Hub

object CassandraDrawings {
  // TODO: Extract to own class, and reuse in DrawingInMemory
  private case class DrawingState(lastSequenceNr: Long = 0) {
    def handle(now: Instant, command: DrawCommand): Option[DrawEvent] = {
      def emit(body: DrawEventBody) = Some(this.emit(now, body))

      command.body match {
        case StartScribble(id, points, _) =>
          emit(ScribbleStarted(id, points.map(p => Point(p.x, p.y))))
        case ContinueScribble(id, points, _) =>
          // TODO: Verify scribble exists
          emit(ScribbleContinued(id, points.map { p => Point(p.x, p.y) }))
        case DeleteObject(id, _) =>
          // TODO: Verify scribble OR icon exists
          emit(ObjectDeleted(id))
        case MoveObject(id, Some(position), _) =>
          // TODO: Verify scribble OR icon exists
          emit(ObjectMoved(id, Some(position)))
        case CreateIcon(id, position, category, name, _) =>
          emit(IconCreated(id, Some(position), Some(category), Some(name)))
        case LabelObject(id, label, _) =>
          // TODO: verify icon exists
          emit(ObjectLabelled(id, label))
        case _ =>
          None
      }
    }

    def update(event: DrawEvent) = copy(
      lastSequenceNr = event.sequenceNr
    )

    def exists = lastSequenceNr > 0

    def handleCreate(now: Instant) = emit(now, DrawingCreated())

    private def emit(now: Instant, body: DrawEventBody) =
      DrawEvent(lastSequenceNr + 1, body, Some(now.toEpochMilli()))
  }

  val make = for {
    session <- ZIO.service[ZCqlSession]
  } yield new Drawings {
    val layer = ZLayer.succeed(session)

    def getDrawing(id: UUID): IO[DrawingError, Drawing] = for {
      startState <- currentDrawingEventsAfter(id, 0).runFold(DrawingState())(_.update(_))
      state <- Ref.make(startState)
      semaphore <- Semaphore.make(1)
      hub <- Hub.unbounded[DrawEvent]
      emit = (event: DrawEvent) => storeEvent(id, event) *> state.update(_.update(event))
      _ <- if (startState.exists) ZIO.unit else for {
        event <- Clock.instant.map(startState.handleCreate)
        _ <- emit(event)
      } yield ()
    } yield new Drawing {
      override def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit] = {
        semaphore.withPermit {
          for {
            now <- Clock.instant
            optEvent <- state.get.map(_.handle(now, command))
            _ <- ZIO.collectAll(optEvent.map { event =>
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

    /*
    def drawingEventsAfter(id: UUID, sequenceNr: Long) = ZStream.unwrap {
      for {
        lastSequenceNr <- Ref.make(sequenceNr)
      } yield {
        // FIXME this is fun but won't do (hammers cassandra or too long latency). Just emit synchronously to all open streams, just like drawing in memory. We'll do something more fancy once we have shardcake.
        ZStream.unwrap{
          lastSequenceNr.get.map { nr =>
            currentDrawingEventsAfter(id, nr).chunks.tap { chunk =>
              if (chunk.isEmpty) ZIO.unit else lastSequenceNr.set(chunk.last.sequenceNr)
            }.flattenChunks
          }
        }.repeat(Schedule.spaced(5.seconds))
      }
     }
     */

  }

  private def toError(x: CassandraException) = DrawingError(x.toString())
}
