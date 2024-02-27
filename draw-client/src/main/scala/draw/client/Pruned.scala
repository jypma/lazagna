package draw.client

import zio.ZIO
import zio.lazagna.eventstore.EventStore

import draw.data.drawevent.{DrawEvent, IconCreated, ObjectDeleted, ObjectMoved, ScribbleContinued, ScribbleStarted}
import org.scalajs.dom

object Pruned {
  type E = DrawEvent
  type Err = dom.DOMException | dom.ErrorEvent
  type Backend = EventStore[E, Err]

  case class ScribbleState(started: DrawEvent, moved: Option[DrawEvent])
  case class IconState(started: DrawEvent, moved: Option[DrawEvent])

  case class State(
    scribbles: Map[String, ScribbleState] = Map.empty,
    icons: Map[String, IconState] = Map.empty
  ) {
    private def update(scribbleId: String, state: ScribbleState) = copy(
      scribbles = scribbles + (scribbleId -> state)
    )

    private def update(iconId: String, state: IconState) = copy(
      icons = icons + (iconId -> state)
    )

    /** Apply an already pruned event from storage */
    def recover(event: DrawEvent): State = {
      event match {
        case e@DrawEvent(_, ScribbleStarted(scribbleId, _, _), _, _, _) =>
          update(scribbleId, ScribbleState(e, None))
        case e@DrawEvent(_, IconCreated(id, _, _, _, _), _, _, _) =>
          update(id, IconState(e, None))
        case e@DrawEvent(_, ObjectMoved(id, _, _), _, _, _) if scribbles.contains(id) =>
          update(id, scribbles(id).copy(moved = Some(e)))
        case e@DrawEvent(_, ObjectMoved(id, _, _), _, _, _) if icons.contains(id) =>
          update(id, icons(id).copy(moved = Some(e)))
        case other =>
          println("??? Unexpected recovery event: " + other)
          this
      }
    }

    /** Apply a new event from source, potentially modifying pruned storage */
    // TEST: We need lots of tests here. Once this gets serious.
    def prune(event: DrawEvent): ZIO[Backend, Err, State] = for {
      storage <- ZIO.service[Backend]
      res <- event match {
        case e@DrawEvent(_, ScribbleStarted(scribbleId, startPoints, _), _, _, _) =>
          storage.publish(e).as(
            update(scribbleId, ScribbleState(e, None))
          )

        case DrawEvent(_, ScribbleContinued(scribbleId, newPoints, _), _, _, _) =>
          scribbles.get(scribbleId) match {
            case Some(ScribbleState(DrawEvent(oldSequenceNr, s@ScribbleStarted(_, startPoints, _), _, _, _), moved)) =>
              val newEvent = event.copy(body = s.copy(points = startPoints ++ newPoints))
              storage.publishAndReplace(newEvent, oldSequenceNr).as(copy(
                scribbles = scribbles + (scribbleId -> ScribbleState(newEvent, moved))
              ))
            case _ =>
              // Continued without started, let's treat it as a started.
              println("??? Continued without started")
              val newEvent = event.copy(body = ScribbleStarted(scribbleId, newPoints))
              storage.publish(newEvent).as(
                update(scribbleId, ScribbleState(newEvent, None))
              )
          }

        case e@DrawEvent(_, IconCreated(id, _, _, _, _), _, _, _) =>
          storage.publish(e).as(
            update(id, IconState(e, None))
          )

        case DrawEvent(_, ObjectDeleted(id, _), _, _, _) =>
          // We don't need to keep the Deleted event itself, since we're removing all traces of the object.
          scribbles.get(id) match {
            case Some(ScribbleState(DrawEvent(sequenceNr, _, _, _, _), moved)) =>
              val deleteMoved = moved.map(m => storage.delete(m.sequenceNr)).getOrElse(ZIO.unit)
              (storage.delete(sequenceNr) *> deleteMoved).as(copy(
                scribbles = scribbles - id
              ))
            case _ =>
              icons.get(id) match {
                case Some(IconState(DrawEvent(sequenceNr, _, _, _, _), moved)) =>
                  val deleteMoved = moved.map(m => storage.delete(m.sequenceNr)).getOrElse(ZIO.unit)
                  (storage.delete(sequenceNr) *> deleteMoved).as(copy(
                    icons = icons - id
                  ))
                case _ =>
                  ZIO.succeed(this)
              }
          }

        case e@DrawEvent(_, ObjectMoved(id, Some(position), _), _, _, _) =>
          scribbles.get(id) match {
            case Some(ScribbleState(started, Some(DrawEvent(oldSequenceNr, _, _, _, _)))) =>
              storage.publishAndReplace(e, oldSequenceNr).as(
                update(id, ScribbleState(started, Some(e)))
              )

            case Some(ScribbleState(started, _)) =>
              storage.publish(e).as(
                update(id, ScribbleState(started, Some(e)))
              )

            case _ =>
              icons.get(id) match {
                case Some(IconState(started, Some(DrawEvent(oldSequenceNr, _, _, _, _)))) =>
                  storage.publishAndReplace(e, oldSequenceNr).as(
                    update(id, IconState(started, Some(e)))
                  )

                case Some(IconState(started, _)) =>
                  storage.publish(e).as(
                    update(id, IconState(started, Some(e)))
                  )

                case _ =>
                  println("??? Moved without started")
                  ZIO.succeed(this)
              }
          }

        case other =>
          println("??? Ignoring " + other)
          ZIO.succeed(this)
      }
    } yield res
  }
}
