package draw.client

import zio.ZIO
import zio.lazagna.Consumeable
import zio.stream.SubscriptionRef

import draw.data.drawcommand.DrawCommand
import org.scalajs.dom
import org.scalajs.dom.SVGRect
import draw.data.point.Point
import draw.data.drawevent.ScribbleStarted
import draw.data.drawevent.IconCreated
import draw.data.drawevent.DrawEvent
import draw.data.drawevent.DrawEventBody
import draw.data.drawevent.ObjectDeleted
import draw.data.drawevent.ScribbleContinued
import draw.data.drawevent.ObjectMoved
import draw.data.drawevent.ObjectLabelled
import draw.data.ObjectState
import zio.Scope
import zio.lazagna.Setup

trait Drawing {
  import Drawing._

  def connectionStatus: Consumeable[Drawing.ConnectionStatus]
  def perform(command: DrawCommand): ZIO[Any, Nothing, Unit]
  def initialVersion: Long
  def currentVersion: Consumeable[Long]
  def viewport: SubscriptionRef[Drawing.Viewport]
  def initialObjectStates: Consumeable[ObjectState[_]] // Returns initial object state for each object
  def objectState(id: String): Consumeable[ObjectState[_]] // Returns state for that object
  def latency: Consumeable[Long]
  /** The set of currently selected objects */
  def selection: SubscriptionRef[Set[String]]

  def currentSelectionState: ZIO[Scope & Setup, Nothing, Set[ObjectState[_]]] =
    selection.get.flatMap(s => ZIO.collectAll(s.map(id => objectState(id).runHead))).map(_.flatten)
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

    def toSvgViewBox: String = {
      s"${left} ${top} ${factor * 1000.0} ${factor / aspectRatio * 1000.0}"
    }
  }

  object Viewport {
    private def aspectRatio = dom.window.innerWidth / dom.window.innerHeight

    def fit(rect: dom.SVGRect): Viewport = {
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
