package draw.client

import zio.UIO
import zio.lazagna.Consumeable
import zio.stream.SubscriptionRef

import draw.data.drawcommand.DrawCommand
import draw.data.{ObjectState, ObjectStateBody}
import org.scalajs.dom
import org.scalajs.dom.SVGRect

trait Drawing {
  import Drawing._

  def connectionStatus: Consumeable[Drawing.ConnectionStatus]
  def perform(command: DrawCommand): UIO[Unit]
  def initialVersion: Long
  def currentVersion: Consumeable[Long]
  def viewport: SubscriptionRef[Drawing.Viewport]
  def initialObjectStates: Consumeable[ObjectState[_]] // Returns initial object state for each object
  def objectState(id: String): Consumeable[ObjectState[_]] // Returns state for that object
  def follow[T <: ObjectStateBody](id: String): Consumeable[ObjectState[T]] =
    objectState(id).asInstanceOf[Consumeable[ObjectState[T]]]
  def objectState[T <: ObjectStateBody](initial: ObjectState[T]): Consumeable[ObjectState[T]] =
    objectState(initial.id).asInstanceOf[Consumeable[ObjectState[T]]]
  def latency: Consumeable[Long]
}

object Drawing {
  sealed trait ConnectionStatus
  case object Connected extends ConnectionStatus
  case object Disconnected extends ConnectionStatus

  case class Viewport(left: Double = 0, top: Double = 0, factor: Double = 1.0) {
    import Viewport._

    def pan(dx: Double, dy: Double) = copy(left = left + dx, top = top + dy)

    def zoom(f: Double, mx: Double, my: Double) = {
      if (f < 1 && factor < 0.01 || f > 1 && factor > 100) {
        this
      } else {
        copy(
          factor = factor * f,
          left = left - ((mx - left) * (f - 1)),
          top = top - ((my - top)) * (f - 1))
      }
    }

    /** Renders this Viewport as an SVG tag "viewBox" attribute value */
    def toSvgViewBox(aspectRatio: Double = windowAspectRatio): String = {
      s"${left} ${top} ${factor * 1000.0} ${factor / aspectRatio * 1000.0}"
    }
  }

  object Viewport {
    private def windowAspectRatio = dom.window.innerWidth / dom.window.innerHeight

    def fit(rect: dom.SVGRect, aspectRatio: Double = windowAspectRatio): Viewport = {
      // Add a bit of margin
      rect.x -= 20
      rect.y -= 20
      rect.width += 40
      rect.height += 40

      val xFactor = rect.width / 1000.0
      val yFactor = rect.height / (1000.0 / aspectRatio)
      val factor = Math.max(0.1, Math.max(xFactor, yFactor))
      // We'll now have width factor * 1000, height factor / aspectRatio * 1000
      // Our rect has actual height rect.height
      // So we have (factor / aspectRatio * 1000) - rect.height left over
      val ySpace = Math.max(0, (factor / aspectRatio * 1000) - rect.height)
      val xSpace = Math.max(0, (factor * 1000) - rect.width)
      Viewport(
        factor = factor,
        left = rect.x - (xSpace / 2),
        top = rect.y - (ySpace / 2)
      )
    }
  }
}
