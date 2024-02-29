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

trait Drawing {
  import Drawing._

  def connectionStatus: Consumeable[Drawing.ConnectionStatus]
  def perform(command: DrawCommand): ZIO[Any, Nothing, Unit]
  def initialVersion: Long
  def currentVersion: Consumeable[Long]
  def viewport: SubscriptionRef[Drawing.Viewport]
  def initialObjectStates: Consumeable[ObjectState[_]] // Returns initial object state for each object
  def objectState(id: String): Consumeable[ObjectState[_]] // Returns state for that object
}

object Drawing {
  sealed trait ConnectionStatus
  case object Connected extends ConnectionStatus
  case object Disconnected extends ConnectionStatus

  sealed trait ObjectStateBody {
    def update(event: DrawEventBody) = this
  }

  case class ObjectState[T <: ObjectStateBody](id: String, sequenceNr: Long, deleted: Boolean, body: T) {
    def update(event: DrawEvent): ObjectState[T] = copy(
      sequenceNr = event.sequenceNr,
      deleted = event.body.isInstanceOf[ObjectDeleted],
      body = body.update(event.body).asInstanceOf[T]
    )
  }

  case class ScribbleState(position: Point, points: Seq[Point]) extends ObjectStateBody {
    override def update(event: DrawEventBody) = event match {
      case ScribbleContinued(_, addedPoints, _) =>
        copy(points = points ++ addedPoints)
      case ObjectMoved(_, Some(newPosition), _) =>
        copy(position = newPosition)
      case _ => this
    }

  }
  case class IconState(position: Point, symbol: SymbolRef, label: String) extends ObjectStateBody {
    override def update(event: DrawEventBody) = event match {
      case ObjectMoved(_, Some(newPosition), _) =>
        copy(position = newPosition)
      case ObjectLabelled(_, newLabel, _) =>
        copy(label = newLabel)
      case _ => this
    }
  }

  case class DrawingState(objects: Map[String, ObjectState[_]]) {
    private def set(id: String, state: ObjectState[_], isNew: Boolean) =
      ((Some(state), isNew), copy(objects = objects + (id -> state)))
    private def update(id: String, event: DrawEvent) = {
      objects.get(id).map { state =>
        set(id, state.update(event), false)
      }.getOrElse(((None, false), this))
    }

    /** Returns the new drawing state, new object state, and whether that object is new */
    def update(event: DrawEvent): ((Option[ObjectState[_]], Boolean), DrawingState) = event match {
      case DrawEvent(sequenceNr, ScribbleStarted(id, points, _), _, _, _) =>
        set(id, ObjectState(id, sequenceNr, false, ScribbleState(Point(0,0), points)), true)
      case DrawEvent(sequenceNr, IconCreated(id, optPos, Some(category), Some(name), _), _, _, _) =>
        set(id, ObjectState(id, sequenceNr, false, IconState(optPos.getOrElse(Point(0,0)), SymbolRef(SymbolCategory(category), name), "")), true)
      case DrawEvent(_, ScribbleContinued(id, _, _), _, _, _) =>
        update(id, event)
      case DrawEvent(_, ObjectMoved(id, _, _), _, _, _) =>
        update(id, event)
      case DrawEvent(_, ObjectLabelled(id, _, _), _, _, _) =>
        update(id, event)
      case DrawEvent(_, ObjectDeleted(id, _), _, _, _) =>
        update(id, event)

      case _ =>
        ((None, false), this)
    }
  }

  case class Viewport(left: Double = 0, top: Double = 0, factor: Double = 1.0) {
    import Viewport._

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
