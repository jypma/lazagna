package draw.client

import zio.ZIO
import zio.lazagna.Consumeable
import zio.stream.SubscriptionRef

import draw.data.drawcommand.DrawCommand
import draw.data.drawevent.DrawEvent
import org.scalajs.dom.SVGRect
import org.scalajs.dom

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
    private def aspectRatio = dom.window.innerWidth / dom.window.innerHeight

    def fit(rect: dom.SVGRect) = {
      val xFactor = rect.width / 1000.0
      val yFactor = rect.height / (1000.0 / aspectRatio)
      val factor = Math.max(0.1, Math.max(xFactor, yFactor))
      // We'll now have width factor * 1000, height factor / aspectRatio * 1000
      // Our rect has actual height rect.height
      // So we have (factor / aspectRatio * 1000) - rect.height left over
      val ySpace = Math.max(0, (factor / aspectRatio * 1000) - rect.height)
      val xSpace = Math.max(0, (factor * 1000) - rect.width)
      copy(
        factor = factor,
        left = rect.x - (xSpace / 2),
        top = rect.y - (ySpace / 2)
      )
    }

    def pan(dx: Double, dy: Double) = copy(left = left + dx, top = top + dy)

    def zoom(f: Double, mx: Double, my: Double) = {
      copy(
        factor = factor * f,
        left = left - ((mx - left) * (f - 1)),
        top = top - ((my - top)) * (f - 1))
    }

    def toSvgViewBox: String = {
      s"${left} ${top} ${factor * 1000.0} ${factor / aspectRatio * 1000.0}"
    }
  }
}
