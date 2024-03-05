package draw.server.drawing

import java.time.Instant
import java.util.UUID

import scala.collection.Searching.{Found, InsertionPoint}

import zio.stream.{SubscriptionRef, ZStream}
import zio.{Clock, Ref, ZIO, ZLayer}

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent

import Drawings.DrawingError
import draw.data.DrawingState

case class DrawingStorage(state: DrawingState = DrawingState(), events: Seq[DrawEvent] = Seq.empty) {
  def size = events.size

  def handle(now: Instant, command: DrawCommand): (Option[DrawEvent], DrawingStorage) = {
    val event = state.handle(now, command)
    (event, DrawingStorage(
      state = event.map(state.update).map(_._2).getOrElse(state),
      events = events ++ event.toSeq
    ))
  }

  def handleCreate(now: Instant) = {
    val event = state.handleCreate(now)
    copy(
      state = state.update(event)._2,
      events = events :+ event
    )
  }
}

case class DrawingInMemory(storage: SubscriptionRef[DrawingStorage]) extends Drawing {

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
  // TODO: Remove drawing from memory
  override def getDrawing(id: UUID) = {
    storage.updateSomeAndGetZIO {
      case map if map.size > 100 =>
        ZIO.fail(DrawingError("Too many drawings"))
      case map if !map.contains(id) => for {
        drawStorage <- SubscriptionRef.make(DrawingStorage())
        now <- Clock.instant
        _ <- drawStorage.update(_.handleCreate(now))
        drawing = DrawingInMemory(drawStorage)
      } yield map + (id -> drawing)
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
