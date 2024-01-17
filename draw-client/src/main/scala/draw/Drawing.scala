package draw

import zio.lazagna.Consumeable
import zio.{Clock, Hub, ZIO, ZLayer}

import draw.data.drawevent.{DrawEvent, Point, ScribbleContinued, ScribbleDeleted, ScribbleStarted}

trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, DrawCommand.Failed, Unit]
  def events: Consumeable[DrawEvent]
}

case class DrawingInMemory(storage: Hub[DrawEvent]) extends Drawing {
  override def perform(command: DrawCommand): ZIO[Any, DrawCommand.Failed, Unit] = {
    for {
      now <- Clock.instant
      _ <- command match {
        case DrawCommand.StartScribble(id, start) =>
          storage.publish(DrawEvent(
            now.toEpochMilli(),
            ScribbleStarted(
              id, Some(Point(start.x, start.y))
            )
          ))

        case DrawCommand.ContinueScribble(id, points) =>
          storage.publish(DrawEvent(
            now.toEpochMilli(),
            ScribbleContinued(
              id, points.map { p => Point(p.x, p.y) }
            )
          ))

        case DrawCommand.DeleteScribble(id) =>
          storage.publish(DrawEvent(
            now.toEpochMilli(),
            ScribbleDeleted(id)
          ))
      }
    } yield ()
  }

  override def events: Consumeable[DrawEvent] = {
    Consumeable.fromHub(storage)
  }
}

object Drawing {
  val live = ZLayer.fromZIO {
    for {
      storage <- Hub.bounded[DrawEvent](16)
    } yield DrawingInMemory(storage)
  }
}
