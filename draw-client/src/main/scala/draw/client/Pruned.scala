package draw.client

import zio.ZIO
import zio.lazagna.eventstore.EventStore

import draw.data.drawevent.{DrawEvent, ScribbleContinued, ScribbleDeleted, ScribbleStarted, ObjectMoved}
import org.scalajs.dom

object Pruned {
  type E = DrawEvent
  type Err = dom.DOMException | dom.ErrorEvent
  type Backend = EventStore[E, Err]

  case class ScribbleState(started: DrawEvent, moved: Option[DrawEvent]) {

  }

  case class State(scribbles: Map[String, ScribbleState] = Map.empty) {
    private def update(scribbleId: String, state: ScribbleState) = copy(
      scribbles = scribbles + (scribbleId -> state)
    )

    /** Apply an already pruned event from storage */
    def recover(event: DrawEvent): State = {
      event match {
        case e@DrawEvent(_, ScribbleStarted(scribbleId, _, _), _, _, _) =>
          copy(scribbles = scribbles + (scribbleId -> ScribbleState(e, None)))
        case _ =>
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

        case DrawEvent(_, ScribbleDeleted(scribbleId, _), _, _, _) =>
          scribbles.get(scribbleId) match {
            case Some(ScribbleState(DrawEvent(sequenceNr, _, _, _, _), moved)) =>
              // We don't need to keep the Deleted event itself, since we're removing all traces of the scribble.
              val deleteMoved = moved.map(m => storage.delete(m.sequenceNr)).getOrElse(ZIO.unit)
              (storage.delete(sequenceNr) *> deleteMoved).as(copy(
                scribbles = scribbles - scribbleId
              ))
            case _ =>
              ZIO.succeed(this)
          }

        case e@DrawEvent(_, ObjectMoved(scribbleId, Some(position), _), _, _, _) =>
          scribbles.get(scribbleId) match {
            case Some(ScribbleState(started, Some(DrawEvent(oldSequenceNr, _, _, _, _)))) =>
              storage.publishAndReplace(e, oldSequenceNr).as(
                update(scribbleId, ScribbleState(started, Some(e)))
              )

            case Some(ScribbleState(started, _)) =>
              storage.publish(e).as(
                update(scribbleId, ScribbleState(started, Some(e)))
              )

            case _ =>
              println("??? Moved without started")
              ZIO.succeed(this)
          }

        case other =>
          println("??? Ignoring " + other)
          ZIO.succeed(this)
      }
    } yield res
  }
}
