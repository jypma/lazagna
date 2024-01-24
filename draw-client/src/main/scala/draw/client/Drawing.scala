package draw.client

import zio.lazagna.Consumeable
import zio.{Clock, Hub, ZIO, ZLayer}

import draw.data.drawevent.{DrawEvent, ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import draw.data.point.Point
import draw.data.drawcommand.DrawCommand
import draw.data.drawcommand.StartScribble
import draw.data.drawcommand.ContinueScribble
import draw.data.drawcommand.DeleteScribble

trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, Nothing, Unit]
  def events: Consumeable[DrawEvent]
  def eventsAfter(lastSeenSequenceNr: Long): Consumeable[DrawEvent]
}

case class DrawingInMemory(storage: Hub[DrawEvent]) extends Drawing {

  override def perform(command: DrawCommand): ZIO[Any, Nothing, Unit] = {
    for {
      now <- Clock.instant
      _ <- command.body match {
        case StartScribble(id, Some(start), _) =>
          storage.publish(DrawEvent(
            now.toEpochMilli(),
            ScribbleStarted(
              id, Some(Point(start.x, start.y))
            )
          ))

        case ContinueScribble(id, points, _) =>
          storage.publish(DrawEvent(
            now.toEpochMilli(),
            ScribbleContinued(
              id, points.map { p => Point(p.x, p.y) }
            )
          ))

        case DeleteScribble(id, _) =>
          storage.publish(DrawEvent(
            now.toEpochMilli(),
            ScribbleDeleted(id)
          ))

        case _ => ZIO.unit
      }
    } yield ()
  }

  override def events: Consumeable[DrawEvent] = {
    Consumeable.fromHub(storage)
  }

  def eventsAfter(lastSeenSequenceNr: Long): Consumeable[DrawEvent] = ???
}

object Drawing {
  val inMemory = ZLayer.scoped {
    for {
      storage <- Hub.bounded[DrawEvent](16)
    } yield DrawingInMemory(storage)
  }
}
