package draw.client

import zio.ZIO
import zio.lazagna.eventstore.EventStore

import draw.data.drawevent.{DrawEvent, DrawingCreated, IconCreated, LinkCreated, ObjectDeleted, ObjectLabelled, ObjectMoved, ScribbleContinued, ScribbleStarted}
import org.scalajs.dom

object Pruned {
  type E = DrawEvent
  type Err = dom.DOMException | dom.ErrorEvent
  type Backend = EventStore[E, Err]

  trait ObjectState[T <: ObjectState[_]] {
    def started: DrawEvent
    def prune(event: DrawEvent): ZIO[Backend, Err, T]
    def ownedEvents: Seq[DrawEvent]
  }

  case class ScribbleState(started: DrawEvent, moved: Option[DrawEvent] = None) extends ObjectState[ScribbleState] {
    private def startedBody = started.body.asInstanceOf[ScribbleStarted]

    def prune(event: DrawEvent): ZIO[Backend, Err, ScribbleState] = for {
      storage <- ZIO.service[Backend]
      res <- event.body match {
        case ScribbleContinued(id, newPoints, _) =>
          val newEvent = started.copy(
            sequenceNr = event.sequenceNr,
            body = startedBody.copy(points = startedBody.points ++ newPoints)
          )
          storage.publishAndReplace(newEvent, started.sequenceNr).as(copy(started = newEvent))

        case _:ObjectMoved =>
          ZIO.collectAll(moved.map(e => storage.delete(e.sequenceNr))) *>
          storage.publish(event).as(copy(moved = Some(event)))

        case _ => ZIO.succeed(this)
      }
    } yield res

    def ownedEvents = moved.toSeq :+ started
  }

  case class IconState(started: DrawEvent, moved: Option[DrawEvent] = None, labelled: Option[DrawEvent] = None) extends ObjectState[IconState] {
    def prune(event: DrawEvent): ZIO[Backend, Err, IconState] = for {
      storage <- ZIO.service[Backend]
      res <- event.body match {
        case _:ObjectMoved =>
          ZIO.collectAll(moved.map(e => storage.delete(e.sequenceNr))) *>
          storage.publish(event).as(copy(moved = Some(event)))

        case _:ObjectLabelled =>
          ZIO.collectAll(labelled.map(e => storage.delete(e.sequenceNr))) *>
          storage.publish(event).as(copy(labelled = Some(event)))

        case _ => ZIO.succeed(this)
      }
    } yield res

    def ownedEvents = moved.toSeq ++ labelled.toSeq :+ started
  }

  case class LinkState(started: DrawEvent) extends ObjectState[LinkState] {
    def prune(event: DrawEvent): ZIO[Backend, Err, LinkState] = ZIO.succeed(this)

    def ownedEvents = Seq(started)
  }

  case class State(
    scribbles: Map[String, ScribbleState] = Map.empty,
    icons: Map[String, IconState] = Map.empty,
    links: Map[String, LinkState] = Map.empty
  ) {
    private def allObjects = scribbles ++ icons ++ links

    private def update(id: String, state: ScribbleState) = copy(
      scribbles = scribbles + (id -> state)
    )

    private def update(id: String, state: IconState) = copy(
      icons = icons + (id -> state)
    )

    private def update(id: String, state: LinkState) = copy(
      links = links + (id -> state)
    )

    private def updateScribble(id: String, event: DrawEvent) =
      scribbles.get(id).map(
        _.prune(event).map(newState => copy(scribbles = scribbles + (id -> newState)))
      ).getOrElse(ZIO.succeed(this))

    private def updateIcon(id: String, event: DrawEvent) =
      icons.get(id).map(
        _.prune(event).map(newState => copy(icons = icons + (id -> newState)))
      ).getOrElse(ZIO.succeed(this))


    /** Apply an already pruned event from storage */
    def recover(event: DrawEvent): State = {
      event.body match {
        case _:DrawingCreated =>
          this
        case ScribbleStarted(scribbleId, _, _)  =>
          update(scribbleId, ScribbleState(event))
        case IconCreated(id, _, _, _, _, _, _) =>
          update(id, IconState(event))
        case LinkCreated(id, _, _, _, _, _) =>
          update(id, LinkState(event))
        case ObjectMoved(id, _, _) if scribbles.contains(id) =>
          update(id, scribbles(id).copy(moved = Some(event)))
        case ObjectMoved(id, _, _) if icons.contains(id) =>
          update(id, icons(id).copy(moved = Some(event)))
        case ObjectLabelled(id, _, _) if icons.contains(id) =>
          update(id, icons(id).copy(labelled = Some(event)))
        case other =>
          println("??? Unexpected recovery event: " + other)
          this
      }
    }

    /** Apply a new event from source, potentially modifying pruned storage */
    // TEST: We need lots of tests here. Once this gets serious.
    def prune(event: DrawEvent): ZIO[Backend, Err, State] = for {
      storage <- ZIO.service[Backend]
      res <- event.body match {
        case _:DrawingCreated =>
          storage.publish(event).as(this)

        case ScribbleStarted(scribbleId, startPoints, _) =>
          storage.publish(event).as(update(scribbleId, ScribbleState(event)))

        case IconCreated(id, _, _, _, _, _, _) =>
          storage.publish(event).as(update(id, IconState(event)))

        case LinkCreated(id, _, _, _, _, _) =>
          storage.publish(event).as(update(id, LinkState(event)))

        case ScribbleContinued(id, _, _) =>
          updateScribble(id, event)

        case ObjectMoved(id, _, _) =>
          if (scribbles.contains(id)) {
            updateScribble(id, event)
          } else if (icons.contains(id)) {
            updateIcon(id, event)
          } else ZIO.succeed(this)

        case ObjectDeleted(id, _) =>
          // We don't need to keep the Deleted event itself, since we're removing all traces of the object.
          val toDelete = allObjects.get(id).toSeq.flatMap(_.ownedEvents)
          ZIO.collectAll(toDelete.map(e => storage.delete(e.sequenceNr))).as(copy(
            scribbles = scribbles - id,
            icons = icons - id
          ))

        case ObjectLabelled(id, _, _) =>
          updateIcon(id, event)

        case other =>
          println("??? Ignoring " + other)
          ZIO.succeed(this)
      }
    } yield res
  }
}
