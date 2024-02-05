package draw.client

import zio.lazagna.Consumeable
import zio.{Clock, Hub, ZIO, ZLayer}

import draw.data.drawcommand.{ContinueScribble, DeleteScribble, DrawCommand, StartScribble}
import draw.data.drawevent.{DrawEvent, ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import draw.data.point.Point
import zio.stream.SubscriptionRef

// FIXME: events, eventsAfter and initialVersion should just move to EventStore
trait Drawing {
  def perform(command: DrawCommand): ZIO[Any, Nothing, Unit]
  def events: Consumeable[DrawEvent]
  def eventsAfter(lastSeenSequenceNr: Long): Consumeable[DrawEvent]
  def initialVersion: Long
  def viewport: SubscriptionRef[Drawing.Viewport]
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

  def initialVersion = 0L

  def viewport = ???
}

object Drawing {
  case class Viewport(left: Double = 0, top: Double = 0, factor: Double = 1.0) {
    def pan(dx: Double, dy: Double) = copy(left = left + dx, top = top + dy)
    def zoom(f: Double, mx: Double, my: Double) = {
      copy(
        factor = factor * f,
        left = left - ((mx - left) * (f - 1)),
        top = top - ((my - top)) * (f - 1))
    }

    def toSvgViewBox: String = {
      s"${left} ${top} ${factor * 1000.0} ${factor * 1000.0}"
    }
  }

  val inMemory = ZLayer.scoped {
    for {
      storage <- Hub.bounded[DrawEvent](16)
    } yield DrawingInMemory(storage)
  }
}
