package draw.server

import zio.stream.{SubscriptionRef, ZStream}
import zio.{Clock, IO, Ref, ZIO, ZLayer}

import draw.data.drawcommand.{ContinueScribble, DeleteScribble, DrawCommand, StartScribble}
import draw.data.drawevent.{DrawEvent,  ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import draw.data.point.Point

import Drawings.DrawingError

trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit]
  def events: ZStream[Any,Nothing,DrawEvent]
}

trait Drawings {
  def getDrawing(name: String): IO[DrawingError, Drawing]
}

case class DrawingStorage(events: Seq[DrawEvent] = Seq.empty) {
  def size = events.size
  def add(event: DrawEvent) = copy(events = events :+ event.withSequenceNr(events.size))
  def :+(event: DrawEvent) = add(event)
}

case class DrawingInMemory(storage: SubscriptionRef[DrawingStorage]) extends Drawing {

  override def perform(command: DrawCommand): ZIO[Any, DrawingError, Unit] = {
    for {
      now <- Clock.instant
      _ <- storage.get.filterOrFail(_.size < 10000)(DrawingError("Too many events"))
      _ <- command match {
        case DrawCommand(StartScribble(id, Some(start), _), _) =>
          storage.update(_ :+ DrawEvent(
            0,
            ScribbleStarted(
              id, Some(Point(start.x, start.y))
            ),
            Some(now.toEpochMilli())
          ))

        case DrawCommand(ContinueScribble(id, points, _), _) =>
          storage.update(_ :+ DrawEvent(
            0,
            ScribbleContinued(
              id, points.map { p => Point(p.x, p.y) }
            ),
            Some(now.toEpochMilli())
          ))

        case DrawCommand(DeleteScribble(id, _), _) =>
          storage.update(_ :+ DrawEvent(
            0,
            ScribbleDeleted(id),
            Some(now.toEpochMilli())
          ))

        case other =>
          ZIO.fail(DrawingError(s"Invalid or unsupported command: ${other}"))
      }
    } yield ()
  }

  override def events = {
    storage.changes.zipWithPrevious.flatMap { (prev, next) =>
      if (prev.isEmpty) {
        // First element -> emit all previous events
        ZStream.fromIterable(next.events)
      } else {
        // Further elements -> this is a new event, present as the last element
        ZStream(next.events.last)
      }
    }
  }
}

case class DrawingsInMemory(storage: Ref.Synchronized[Map[String,Drawing]]) extends Drawings {
  // TODO: Remove drawing from memory
  override def getDrawing(name: String) = {
    storage.updateSomeAndGetZIO {
      case map if map.size > 100 =>
        ZIO.fail(DrawingError("Too many drawings"))
      case map if !map.contains(name) => for {
        drawStorage <- SubscriptionRef.make(DrawingStorage())
      } yield map + (name -> DrawingInMemory(drawStorage))
    }.map(_(name))
  }
}

object Drawings {
  case class DrawingError(message: String)

  val inMemory = ZLayer.scoped {
    for {
      storage <- Ref.Synchronized.make[Map[String,Drawing]](Map.empty)
    } yield DrawingsInMemory(storage)
  }
}
