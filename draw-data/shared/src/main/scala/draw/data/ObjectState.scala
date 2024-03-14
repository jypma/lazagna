package draw.data

import draw.data.drawevent.DrawEventBody
import draw.geom.Point
import draw.data.drawevent.DrawEvent
import draw.data.drawevent.ObjectDeleted
import draw.data.drawevent.ScribbleContinued
import draw.data.drawevent.ObjectMoved
import draw.data.drawevent.ObjectLabelled

sealed trait ObjectStateBody {
  def update(event: DrawEventBody) = this
}

/** An object that can be moved around, and hence has a position. */
sealed trait Moveable extends ObjectStateBody {
  def position: Point
}
case class ObjectState[+T <: ObjectStateBody](id: String, sequenceNr: Long, deleted: Boolean, body: T) {
  def update(event: DrawEvent): ObjectState[T] = copy(
    sequenceNr = event.sequenceNr,
    deleted = event.body.isInstanceOf[ObjectDeleted],
    body = body.update(event.body).asInstanceOf[T]
  )
}

case class ScribbleState(position: Point, points: Seq[Point]) extends ObjectStateBody with Moveable {
  override def update(event: DrawEventBody) = event match {
    case ScribbleContinued(_, addedPoints, _) =>
      copy(points = points ++ addedPoints.map(Point.fromProtobuf))
    case ObjectMoved(_, Some(newPosition), _) =>
      copy(position = newPosition)
    case _ => this
  }

}

case class IconState(position: Point, symbol: SymbolRef, label: String) extends ObjectStateBody with Moveable {
  override def update(event: DrawEventBody) = event match {
    case ObjectMoved(_, Some(newPosition), _) =>
      copy(position = newPosition)
    case ObjectLabelled(_, newLabel, _) =>
      copy(label = newLabel)
    case _ => this
  }
}

case class LinkState(src: String, dest: String, preferredDistance: Option[Int], preferredAngle: Option[Int]) extends ObjectStateBody {

}
