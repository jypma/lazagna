package draw.data

import draw.data.drawcommand.{ContinueScribble, CreateIcon, DeleteObject, DrawCommand, LabelObject, MoveObject, StartScribble}
import draw.data.drawevent.DrawEvent
import draw.data.drawevent.ScribbleStarted
import draw.data.drawevent.IconCreated
import draw.data.point.Point
import draw.data.drawevent.ScribbleContinued
import draw.data.drawevent.ObjectMoved
import draw.data.drawevent.ObjectLabelled
import draw.data.drawevent.ObjectDeleted
import draw.data.drawevent.DrawEventBody
import draw.data.drawevent.DrawingCreated
import java.time.Instant

case class DrawingState(lastSequenceNr: Long = 0, objects: Map[String, ObjectState[_]] = Map.empty) {
  private def set(sequenceNr: Long, id: String, state: ObjectState[_], isNew: Boolean) =
    ((Some(state), isNew), copy(objects = objects + (id -> state), lastSequenceNr = sequenceNr))

  private def update(id: String, event: DrawEvent) = {
    objects.get(id).map { state =>
      set(event.sequenceNr, id, state.update(event), false)
    }.getOrElse(((None, false), this))
  }

  /** Returns the new drawing state, new object state, and whether that object is new */
  def update(event: DrawEvent): ((Option[ObjectState[_]], Boolean), DrawingState) = event match {
    case DrawEvent(sequenceNr, ScribbleStarted(id, points, _), _, _, _) =>
      set(event.sequenceNr, id, ObjectState(id, sequenceNr, false, ScribbleState(Point(0,0), points)), true)
    case DrawEvent(sequenceNr, IconCreated(id, optPos, Some(category), Some(name), _), _, _, _) =>
      set(event.sequenceNr, id, ObjectState(id, sequenceNr, false, IconState(optPos.getOrElse(Point(0,0)), SymbolRef(SymbolCategory(category), name), "")), true)
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

  /** Returns which events to emit when handling the given command. */
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

  /** Returns whether this drawing exists (has any events), or is completely new */
  def exists: Boolean = lastSequenceNr > 0

  /** Returns the event to emit in case this drawing is completely new (doesn't exist) */
  def handleCreate(now: Instant): DrawEvent = emit(now, DrawingCreated())

  private def emit(now: Instant, body: DrawEventBody) =
    DrawEvent(lastSequenceNr + 1, body, Some(now.toEpochMilli()))
}
