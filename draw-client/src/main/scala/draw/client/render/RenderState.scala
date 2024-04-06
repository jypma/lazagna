package draw.client.render

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.svg.SVGHelper
import zio.stream.{SubscriptionRef, ZStream}
import zio.{Hub, Semaphore, UIO, ZIO, ZLayer}

import draw.data.ObjectState
import draw.geom.Rectangle
import org.scalajs.dom
import draw.data.IconState

case class RenderedObject(state: ObjectState[_], element: dom.Element, boundingBox: Rectangle) {
  def id = state.id
}

trait RenderState {
  /** Emits a new element for each new object */
  def initialObjectStates: Consumeable[RenderedObject]
  def objectState(id: String): Consumeable[RenderedObject]
  def latestSequenceNr: Consumeable[Long]
  def notifyRendered(state: ObjectState[_], element: dom.Element): UIO[Unit]

  /** Returns information about an object that might have been clicked to select it */
  def lookupForSelect(event: dom.MouseEvent): UIO[Option[RenderedObject]] = {
    getTargetObject(event, "selectTarget")
  }

  /** Returns information about an object that might have been clicked to edit it */
  def lookupForEdit(event: dom.MouseEvent): UIO[Option[RenderedObject]] = {
    getTargetObject(event, "editTarget")
  }

  /** The set of currently selected object IDs */
  def selectionIds: Consumeable[Set[String]]
  def selectAlso(ids: Set[String]): UIO[Unit]
  def unselect(ids: Set[String]): UIO[Unit]
  def selectOnly(ids: Set[String]): UIO[Unit]

  /** The set of currently selected objects, and their (changing) state */
  def selection: Consumeable[Set[RenderedObject]]
  def currentSelectionState: ZIO[Any, Nothing, Set[RenderedObject]]

  def selectionBoundingBox: Consumeable[Option[Rectangle]]

  protected def getTargetObject(event: dom.MouseEvent, className: String): UIO[Option[RenderedObject]]

  def expandSelection(direction: Double => Boolean): UIO[Option[RenderedObject]]
}

object RenderState {
  private case class State(
    byId: Map[String, RenderedObject] = Map.empty,
    byElement: Map[dom.Element, RenderedObject] = Map.empty,
    hubs: Map[String, Hub[RenderedObject]] = Map.empty,
    selection: Set[String] = Set.empty
  ) {
    def all: Seq[RenderedObject] = byId.values.filter(!_.state.deleted).toSeq

    def get(id: String) = byId.get(id)

    def + (obj: RenderedObject) = {
      copy(
        byId = byId + (obj.id -> obj),
        byElement = byElement + (obj.element -> obj),
        selection = if (obj.state.deleted) selection - obj.id else selection
      )
    }

    def lookupParent(e: dom.Element, className: String): Option[RenderedObject] = {
      var elem = e
      var count = 3
      var isselectTarget = e.classList.contains(className)
      while (elem.id == "" && count > 0) {
        elem = elem.parentNode.asInstanceOf[dom.Element]
        if (elem.classList.contains(className)) {
          isselectTarget = true
        }
        count -= 1
      }
      Option.when(isselectTarget)(elem).flatMap(byElement.get)
    }

    def selectAlso(ids: Set[String]) = copy(
      selection = selection ++ ids
    )

    def selectOnly(ids: Set[String]) = copy(
      selection = ids
    )

    def unselect(ids: Set[String]) = copy(
      selection = selection -- ids
    )
  }

  def make = for {
    state <- SubscriptionRef.make(State())
    semaphore <- Semaphore.make(1)
    sequenceNr <- SubscriptionRef.make(1L)
    newObjects <- Hub.bounded[RenderedObject](16)
  } yield new RenderState {
    def seen(nr: Long) = sequenceNr.updateSome {
      case n if n <= nr => nr
    }

    def selectionIds: Consumeable[Set[String]] = state.map(_.selection)

    def selectAlso(ids: Set[String]): UIO[Unit] = {
      if (ids.isEmpty) ZIO.unit else {
        state.update(_.selectAlso(ids))
      }
    }
    def unselect(ids: Set[String]): UIO[Unit] = {
      if (ids.isEmpty) ZIO.unit else {
        state.update(_.unselect(ids))
      }
    }
    def selectOnly(ids: Set[String]): UIO[Unit] = {
      state.update(_.selectOnly(ids))
    }

    def selection: Consumeable[Set[RenderedObject]] =
      state.map { s =>
        s.selection.map(s.byId.get).flatMap(_.toSet)
      }

    def currentSelectionState: ZIO[Any, Nothing, Set[RenderedObject]] =
      state.get.map { s =>
        s.selection.map(s.byId.get).flatMap(_.toSet)
      }

    def latestSequenceNr: Consumeable[Long] = sequenceNr

    def initialObjectStates = ZStream.unwrapScoped {
      semaphore.withPermit {
        state.get.map(_.all).flatMap { initial =>
          ZStream.fromHubScoped(newObjects).map { stream =>
            ZStream.fromIterable(initial) ++ stream
          }
        }
      }
    }

    def objectState(id: String) = ZStream.unwrapScoped {
      semaphore.withPermit {
        hub(id).flatMap { hub =>
          state.get
            .map(_.get(id).toSeq)
            .map(ZStream.fromIterable(_) ++ hub)
        }
      }
    }

    private def hub(id: String) = state.updateSomeAndGetZIO {
      case s if !s.hubs.contains(id) =>
        Hub.bounded[RenderedObject](16).map { hub =>
          s.copy(hubs = s.hubs + (id -> hub))
        }
    }.map(_.hubs(id))

    private def getBBox(objState: ObjectState[_], element: dom.Element): UIO[Rectangle] = {
      val padding = 5

      objState.body match {
        case i:IconState if i.iconBoundingBox.isDefined =>
          ZIO.succeed(i.iconBoundingBox.get.expand(padding))
        case _ =>
          val target = Option(element.querySelector(".selectTarget")).getOrElse(element)
          val bbox = new SVGHelper(element.asInstanceOf[dom.SVGElement].ownerSVGElement).svgBoundingBox(target.asInstanceOf[dom.SVGLocatable], padding)
          ZIO.succeed(bbox)
      }
    }

    def notifyRendered(objState: ObjectState[_], element: dom.Element): UIO[Unit] = {
      getBBox(objState, element).flatMap { bbox =>
        if (bbox.width == 10 && bbox.height == 10) {
          println(s"Warning, object ${objState.id} did not render in time for ${objState.body}.")
        }
        val rendered = RenderedObject(objState, element, bbox)

        semaphore.withPermit {
          state.modify { s =>
            val existing = s.get(rendered.id)
            (existing.isEmpty, s + rendered)
          }.flatMap { isNew =>
            hub(rendered.id).flatMap { hub =>
              newObjects.publish(rendered).when(isNew) *> seen(objState.sequenceNr) *> hub.publish(rendered).unit
            }
          }
        }
      }
    }

    def getTargetObject(event: dom.MouseEvent, className: String) = for {
      s <- state.get
    } yield {
      Some(event)
        .map(_.target)
        .collect { case elem: dom.Element => elem }
        .flatMap { e => s.lookupParent(e, className) }
    }

    def selectionBoundingBox = selection.map { selectedObjects =>
      if (selectedObjects.isEmpty) None else Some(
        selectedObjects.tail.foldLeft(selectedObjects.head.boundingBox)(_ union _.boundingBox)
      )
    }

    def expandSelection(direction: Double => Boolean): UIO[Option[RenderedObject]] = for {
      objects <- state.get
    } yield {
      val selected = objects.selection

      if (selected.isEmpty) objects.all.headOption else {
        val selectedObjects = selected.toSeq.flatMap(objects.byId.get)
        val origin = selectedObjects.tail.foldLeft(selectedObjects.head.boundingBox)(_ union _.boundingBox).middle

        val candidates = (objects.all.map(_.id).toSet -- selected).map(objects.byId).filter { c =>
          val angle = c.boundingBox.middle.to(origin).angle
          direction(angle)
        }
        val candidateDistances = candidates.map { c =>
          val distance = c.boundingBox.middle.to(origin).length
          (c, distance)
        }.toSeq.sortBy(_._2)

        candidateDistances.headOption.map(_._1)
      }
    }
  }

  // up is half pi
  // right is pi
  // down is minus half pi
  // left is zero
  object Direction {
    def left(angle: Double) = angle < 0.25 * Math.PI && angle > -0.25 * Math.PI
    def right(angle: Double) = angle > 0.75 * Math.PI || angle < -0.75 * Math.PI
    def down(angle: Double) = angle < -0.25 * Math.PI && angle > -0.75 * Math.PI
    def up(angle: Double) = angle > 0.25 * Math.PI && angle < 0.75 * Math.PI
  }

  val live = ZLayer.fromZIO(make)
}
