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
import draw.data.drawevent.LinkCreated
import java.time.Instant
import draw.data.drawcommand.CreateLink


case class DrawingState(
  lastSequenceNr: Long = 0,
  objects: Map[String, ObjectState[_]] = Map.empty,
  /* Key is object ID, value is link ID */
  objectLinks: DrawingState.Links = Map.empty
) {
  import DrawingState._

  private def alive = objects.view.filter(!_._2.deleted)

  private def set(state: ObjectState[_], isNew: Boolean, newLinks: Links) = (
    (Some(state), isNew),
    copy(
      // TEST: Deletes objects stick around in state (so we know they've been deleted asynchronously). It's OK, they won't be in pruned event storage.
      objects = objects + (state.id -> state),
      lastSequenceNr = state.sequenceNr,
      objectLinks = newLinks
    )
  )

  private def unaffected(event: DrawEvent) =
    ((None, false), copy(lastSequenceNr = event.sequenceNr))

  private def update(id: String, event: DrawEvent, newLinks: Links = objectLinks) = {
    objects.get(id).map { state =>
      set(state.update(event), false, newLinks)
    }.getOrElse(unaffected(event))
  }

  /** Returns the new drawing state, new object state, and whether that object is new */
  def update(event: DrawEvent): ((Option[ObjectState[_]], Boolean), DrawingState) = {
    def create(id: String, body: ObjectStateBody, newLinks: Links = objectLinks) =
      set(ObjectState(id, event.sequenceNr, false, body), true, newLinks)

    event.body match {
      case ScribbleStarted(id, points, _) =>
        create(id, ScribbleState(Point(0,0), points))
      case IconCreated(id, optPos, Some(category), Some(name), _) =>
        create(id, IconState(optPos.getOrElse(Point(0,0)), SymbolRef(SymbolCategory(category), name), ""))
      case LinkCreated(id, src, dest, preferredDistance, preferredAngle, _) =>
        create(id, LinkState(src, dest, preferredDistance, preferredAngle),
          newLinks = objectLinks.add(src, id).add(dest, id))
      case ScribbleContinued(id, _, _) =>
        update(id, event)
      case ObjectMoved(id, _, _)  =>
        update(id, event)
      case ObjectLabelled(id, _, _) =>
        update(id, event)
      case ObjectDeleted(id, _) => alive.get(id).map(_.body) match {
        case Some(LinkState(src, dest, _, _)) =>
          update(id, event, newLinks = objectLinks.remove(src, id).remove(dest, id))
        case _ =>
          update(id, event, newLinks = objectLinks - id)
      }
      case _:DrawingCreated =>
        unaffected(event)
      case _ =>
        println("??? Unhandled: " + event)
        unaffected(event)
    }
  }

  /** Returns which events to emit when handling the given command. */
  def handle(now: Instant, command: DrawCommand): Seq[DrawEvent] = {
    def emit(body: DrawEventBody) = this.emit(now, Seq(body))

    command.body match {
      case StartScribble(id, points, _) =>
        emit(ScribbleStarted(id, points.map(p => Point(p.x, p.y))))
      case ContinueScribble(id, points, _) =>
        // TODO: Verify scribble exists
        emit(ScribbleContinued(id, points.map { p => Point(p.x, p.y) }))
      case DeleteObject(id, _) =>
        // TODO: Verify scribble OR icon exists
        val deleteLinks = objectLinks.getOrElse(id, Set.empty).map(ObjectDeleted(_)).toSeq
        this.emit(now, deleteLinks :+ ObjectDeleted(id))
      case MoveObject(id, Some(position), _) =>
        // TODO: Verify scribble OR icon exists
        emit(ObjectMoved(id, Some(position)))
      case CreateIcon(id, position, category, name, _) =>
        emit(IconCreated(id, Some(position), Some(category), Some(name)))
      case LabelObject(id, label, _) =>
        // TODO: verify icon exists
        emit(ObjectLabelled(id, label))
      case CreateLink(id, src, dest, preferredDistance, preferredAngle, _) =>
        // TODO: verify ids exist
        // TEST: Don't allow adding of link between already linked objects
        if (alive.values.map(_.body).exists {
          case LinkState(s,d,_,_) if s == src && d == dest => true
          case _ => false
        }) {
          println("!!! Duplicate link")
          // We already have this link
          Seq.empty
        } else {
          emit(LinkCreated(id, src, dest, preferredDistance, preferredAngle))
        }
      case _ =>
        Seq.empty
    }
  }

  /** Returns whether this drawing exists (has any events), or is completely new */
  def exists: Boolean = lastSequenceNr > 0

  /** Returns the events to emit in case this drawing is completely new (doesn't exist) */
  def handleCreate(now: Instant): Seq[DrawEvent] = emit(now, Seq(DrawingCreated()))

  private def emit(now: Instant, bodies: Seq[DrawEventBody]) = bodies.map { body =>
    DrawEvent(lastSequenceNr + 1, body, Some(now.toEpochMilli()))
  }

}

object DrawingState {
  type Links = Map[String, Set[String]]

  private[DrawingState] implicit class LinksOp(links: Links) {
    def add(key: String, value: String) =
      links.updated(key, links.getOrElse(key, Set.empty) + value)

    def remove(key: String, value: String) =
      links.updated(key, links.getOrElse(key, Set.empty) - value)
  }
}
