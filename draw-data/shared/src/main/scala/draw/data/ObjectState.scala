package draw.data

import draw.data.drawevent.DrawEventBody
import draw.geom.Point
import draw.data.drawevent.DrawEvent
import draw.data.drawevent.ObjectDeleted
import draw.data.drawevent.ScribbleContinued
import draw.data.drawevent.ObjectMoved
import draw.data.drawevent.ObjectLabelled
import draw.geom.Bounds
import draw.geom.Rectangle
import draw.data.drawevent.LinkEdited

sealed trait ObjectStateBody {
  def update(event: DrawEventBody) = this
  def updateDeps(deps: String => Moveable) = this
  def boundingBoxes: Seq[Rectangle]
  def boundingBox: Rectangle = boundingBoxes.reduce(_.union(_))
}

/** An object that can be moved around, and hence has a position. */
sealed trait Moveable extends ObjectStateBody {
  def position: Point
}

case class ObjectState[T <: ObjectStateBody](id: String, sequenceNr: Long, deleted: Boolean, body: T) {
  type Body = T

  def update(event: DrawEvent, deps: String => Moveable): ObjectState[T] = copy(
    sequenceNr = event.sequenceNr,
    deleted = event.body.isInstanceOf[ObjectDeleted],
    body = body.update(event.body).updateDeps(deps).asInstanceOf[T]
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

  override def boundingBoxes = {
    val minX = points.view.map(_.x).min
    val maxX = points.view.map(_.x).max
    val minY = points.view.map(_.y).min
    val maxY = points.view.map(_.y).max
    Seq(Rectangle(Point(minX, minY), maxX - minX, maxY - minY).move(position.x, position.y))
  }
}

case class IconState(position: Point, symbol: SymbolRef, label: String, bounds: Option[Bounds] = None, labelBounds: Option[Rectangle] = None) extends ObjectStateBody with Moveable {
  override def update(event: DrawEventBody) = event match {
    case ObjectMoved(_, Some(newPosition), _) =>
      copy(position = newPosition)
    case ObjectLabelled(_, newLabel, Some(width), Some(height), Some(yOffset), _) =>
      copy(label = newLabel, labelBounds = Some(Rectangle(Point(-width / 2, yOffset), width, height)))
    case ObjectLabelled(_, newLabel, _, _, _, _) =>
      copy(label = newLabel)
    case _ => this
  }

  def withBounds(width: Option[Double], height:Option[Double]): IconState = {
    width.zip(height).map { (w,h) => copy(
      bounds = Some(Bounds(w, h))
    )}.getOrElse(this)
  }

  def iconBoundingBox: Option[Rectangle] = bounds.map(_.middleAt(position))

  override def boundingBoxes = {
    val iconB = bounds.map(_.middleAt(position))
    val labelB = labelBounds.map(_.move(position.x, position.y + IconState.mediumSize / 2))
    iconB.toSeq ++ labelB.toSeq
  }
}
object IconState {
  val mediumSize = 64
}

case class LinkState(src: String, dest: String, preferredDistance: Option[Int], preferredAngle: Option[Int],
  srcState: Moveable, destState: Moveable) extends ObjectStateBody {

  override def update(event: DrawEventBody) = event match {
    case LinkEdited(_, distance, angle, _) =>
      copy(preferredDistance = distance, preferredAngle = angle)
    case _ => this
  }

  override def updateDeps(deps: String => Moveable): ObjectStateBody = copy(
    srcState = deps(src),
    destState = deps(dest)
  )

  override def boundingBoxes: Seq[Rectangle] = {
    Seq(Rectangle(srcState.position, destState.position))
  }
}
