package draw.client

import zio.ZIO
import zio.lazagna.eventstore.EventStore

import draw.data.drawevent.{DrawEvent, ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import org.scalajs.dom

object Pruned {
  type E = DrawEvent
  type Err = dom.DOMException | dom.ErrorEvent
  type Backend = EventStore[E, Err]

  case class State(scribbles: Map[String, DrawEvent] = Map.empty) {
    /** Apply an already pruned event from storage */
    def recover(event: DrawEvent): State = {
      event match {
        case e@DrawEvent(_, ScribbleStarted(scribbleId, _, _), _, _, _) =>
          copy(scribbles = scribbles + (scribbleId -> e))
        case _ =>
          this
      }
    }

    /** Apply a new event from source, potentially modifying pruned storage */
    def prune(event: DrawEvent): ZIO[Backend, Err, State] = for {
      storage <- ZIO.service[Backend]
      res <- event match {
        case e@DrawEvent(_, ScribbleStarted(scribbleId, startPoints, _), _, _, _) =>
          storage.publish(e).map { _ =>
            copy(scribbles = scribbles + (scribbleId -> e))
          }
        case e@DrawEvent(_, ScribbleContinued(scribbleId, newPoints, _), _, _, _) =>
          scribbles.get(scribbleId) match {
            case Some(e@DrawEvent(oldSequenceNr, s@ScribbleStarted(_, startPoints, _), _, _, _)) =>
              val newEvent = event.copy(body = s.copy(points = startPoints ++ newPoints))
              storage.publishAndReplace(newEvent, oldSequenceNr).as(copy(
                scribbles = scribbles + (scribbleId -> newEvent)
              ))
            case _ =>
              // Continued without started, let's treat it as a started.
              println("??? Continued without started")
              val newEvent = event.copy(body = ScribbleStarted(scribbleId, newPoints))
              storage.publish(newEvent).as(copy(
                scribbles = scribbles + (scribbleId -> newEvent)
              ))
          }
        case e@DrawEvent(_, ScribbleDeleted(scribbleId, _), _, _, _) =>
          scribbles.get(scribbleId) match {
            case Some(DrawEvent(sequenceNr, _, _, _, _)) =>
              // We don't need to keep the Deleted event itself, since we're removing all traces of the scribble.
              storage.delete(sequenceNr).as(copy(
                scribbles = scribbles - scribbleId
              ))
            case _ =>
              ZIO.succeed(this)
          }
        case other =>
          ZIO.succeed(this)
      }
    } yield res
  }
}
