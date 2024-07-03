package draw.server.drawing

import java.time.Instant
import java.util.UUID

import scala.collection.Searching.{Found, InsertionPoint}

import zio.stream.{SubscriptionRef, ZStream}
import zio.{Clock, Ref, ZIO, ZLayer}

import draw.data.DrawingState
import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent

import Drawings.DrawingError

case class DrawingStorage(state: DrawingState = DrawingState(), events: Seq[DrawEvent] = Seq.empty) {
  def size = events.size

  def handle(now: Instant, command: DrawCommand): (Seq[DrawEvent], DrawingStorage) = {
    val emitted = state.handle(now, command)
    (emitted, doHandle(emitted))
  }

  def handleCreate(now: Instant) = {
    doHandle(state.handleCreate(now))
  }

  private def doHandle(emitted: Seq[DrawEvent]) = copy(
    state = emitted.foldLeft(state)((s,e) => s.update(e)._2),
    events = events ++ emitted
  )
}

case class DrawingInMemory(storage: SubscriptionRef[DrawingStorage]) extends Drawing {
  override def getState = storage.get.map(_.state)

  override def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit] = {
    for {
      now <- Clock.instant
      _ <- storage.get.filterOrFail(_.size < 10000)(DrawingError("Too many events"))
      _ <- storage.modify(_.handle(now, command))
    } yield ()
  }

  override def events = eventsAfter(-1)

  override def eventsAfter(afterSequenceNr: Long) = {
    storage.changes.zipWithPrevious.flatMap { (prev, next) =>
      if (prev.isEmpty) {
        // First element -> emit all previous events
        if (afterSequenceNr == -1)
          ZStream.fromIterable(next.events)
        else {
          val toDrop = next.events.view.map(_.sequenceNr).search(afterSequenceNr) match {
            case Found(idx) => idx + 1
            case InsertionPoint(idx) => idx
          }
          ZStream.fromIterable(next.events.drop(toDrop))
        }
      } else {
        // Further elements -> this is a new event, present as the last element
        ZStream(next.events.last)
      }
    }
  }

  override def version = storage.get.map(_.events.size)
}

case class InMemoryDrawings(storage: Ref.Synchronized[Map[UUID,Drawing]]) extends Drawings {
  override def list = ZStream.unwrap(storage.get.map(s => ZStream.fromIterable(s.keySet.map { id =>
    Drawings.DrawingRef(id, id.toString)
  })))

  override def makeDrawing = {
    val id = UUID.randomUUID()
    storage.updateZIO { map =>
      for {
        drawStorage <- SubscriptionRef.make(DrawingStorage())
        now <- Clock.instant
        _ <- drawStorage.update(_.handleCreate(now))
        drawing = DrawingInMemory(drawStorage)
      } yield map + (id -> drawing)
    }.as(id)
  }

  override def getDrawing(id: UUID) = {
    storage.updateSomeAndGetZIO {
      case map if map.size > 100 =>
        ZIO.fail(DrawingError("Too many drawings"))
      case map if !map.contains(id) =>
        ZIO.fail(DrawingError("Drawing not found: " + id))
    }.map(_(id))
  }
}

object InMemoryDrawings {
  val make = ZLayer.scoped {
    for {
      storage <- Ref.Synchronized.make[Map[UUID,Drawing]](Map.empty)
    } yield InMemoryDrawings(storage)
  }
}
