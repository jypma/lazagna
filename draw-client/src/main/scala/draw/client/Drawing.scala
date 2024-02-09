package draw.client

import zio.ZIO
import zio.lazagna.Consumeable
import zio.stream.SubscriptionRef

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent

trait Drawing {
  def connectionStatus: Consumeable[Drawing.ConnectionStatus]
  def perform(command: DrawCommand): ZIO[Any, Nothing, Unit]
  def events: Consumeable[DrawEvent]
  def eventsAfter(lastSeenSequenceNr: Long): Consumeable[DrawEvent]
  def initialVersion: Long
  def viewport: SubscriptionRef[Drawing.Viewport]
}

object Drawing {
  sealed trait ConnectionStatus
  case object Connected extends ConnectionStatus
  case object Disconnected extends ConnectionStatus

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
}
