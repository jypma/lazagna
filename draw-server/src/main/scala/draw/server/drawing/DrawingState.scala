package draw.server.drawing

import java.time.Instant

import draw.data.drawcommand.{ContinueScribble, CreateIcon, DeleteObject, DrawCommand, LabelObject, MoveObject, StartScribble}
import draw.data.drawevent.{DrawEvent, DrawEventBody, DrawingCreated, IconCreated, ObjectDeleted, ObjectLabelled, ObjectMoved, ScribbleContinued, ScribbleStarted}
import draw.data.point.Point

case class DrawingState(lastSequenceNr: Long = 0) {
  def handle(now: Instant, command: DrawCommand): Option[DrawEvent] = {
    def emit(body: DrawEventBody) = Some(this.emit(now, body))

    command.body match {
      case StartScribble(id, points, _) =>
        emit(ScribbleStarted(id, points.map(p => Point(p.x, p.y))))
      case ContinueScribble(id, points, _) =>
        // TODO: Verify scribble exists
        emit(ScribbleContinued(id, points.map { p => Point(p.x, p.y) }))
      case DeleteObject(id, _) =>
        // TODO: Verify scribble OR icon exists
        emit(ObjectDeleted(id))
      case MoveObject(id, Some(position), _) =>
        // TODO: Verify scribble OR icon exists
        emit(ObjectMoved(id, Some(position)))
      case CreateIcon(id, position, category, name, _) =>
        emit(IconCreated(id, Some(position), Some(category), Some(name)))
      case LabelObject(id, label, _) =>
        // TODO: verify icon exists
        emit(ObjectLabelled(id, label))
      case _ =>
        None
    }
  }

  def update(event: DrawEvent) = copy(
    lastSequenceNr = event.sequenceNr
  )

  def exists = lastSequenceNr > 0

  def handleCreate(now: Instant) = emit(now, DrawingCreated())

  private def emit(now: Instant, body: DrawEventBody) =
    DrawEvent(lastSequenceNr + 1, body, Some(now.toEpochMilli()))
}
