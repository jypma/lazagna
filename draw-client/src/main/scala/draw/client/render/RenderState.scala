package draw.client.render

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.stream.ZStream
import zio.{Hub, Ref, Semaphore, UIO, ZIO, ZLayer}

import org.scalajs.dom
import zio.stream.SubscriptionRef
import zio.Scope
import zio.lazagna.Setup
import zio.lazagna.dom.svg.SVGHelper
import draw.geom.Rectangle
import zio.durationInt
import draw.data.ObjectState

case class RenderedObject(state: ObjectState[_], element: dom.Element, boundingBox: Rectangle) {
  def id = state.id
}

trait RenderState {
  /** Emits a new element for each new object */
  def initialObjectStates: Consumeable[RenderedObject]
  def objectState(id: String): Consumeable[RenderedObject]
  /** Emits a new element for any state change */
  def allObjectStates: Consumeable[RenderedObject]
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
  def selection: SubscriptionRef[Set[String]]

  def selectionBoundingBox: Consumeable[Option[Rectangle]]

  def currentSelectionState: ZIO[Scope & Setup, Nothing, Set[RenderedObject]] =
    selection.get.flatMap(s => ZIO.collectAll(s.map(id => objectState(id).runHead))).map(_.flatten)

  protected def getTargetObject(event: dom.MouseEvent, className: String): UIO[Option[RenderedObject]]

  def expandSelection(direction: Double => Boolean): UIO[Option[RenderedObject]]
}

object RenderState {
  private case class State(
    val byId: Map[String, RenderedObject] = Map.empty,
    val byElement: Map[dom.Element, RenderedObject] = Map.empty
  ) {
    def all: Seq[RenderedObject] = byId.values.toSeq

    def get(id: String) = byId.get(id)

    def + (obj: RenderedObject) = copy(
      byId = byId + (obj.id -> obj),
      byElement = byElement + (obj.element -> obj)
    )

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
  }

  def make = for {
    state <- Ref.make(State())
    semaphore <- Semaphore.make(1)
    stateChanges <- Hub.bounded[RenderedObject](16)
    newObjects <- Hub.bounded[RenderedObject](16)
    selectionRef <- SubscriptionRef.make(Set.empty[String])
  } yield new RenderState {
    def selection = selectionRef

    def allObjectStates: Consumeable[RenderedObject] = stateChanges

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
        state.get.map(_.get(id).toSeq)
          .map(ZStream.fromIterable(_) ++ stateChanges.filter(_.id == id))
      }
    }

    def notifyRendered(objState: ObjectState[_], element: dom.Element): UIO[Unit] =
      notifyRendered(objState, element, 10)

    def notifyRendered(objState: ObjectState[_], element: dom.Element, retries: Int): UIO[Unit] = {
      val target = Option(element.querySelector(".selectTarget")).getOrElse(element)
      val bbox = new SVGHelper(element.asInstanceOf[dom.SVGElement].ownerSVGElement).svgBoundingBox(target.asInstanceOf[dom.SVGLocatable], 5)
      if (retries > 0 && bbox.width == 10 && bbox.height == 10) {
        // We're not done rendering yet. Probably a <use> external icon is still being loaded.
        ZIO.suspendSucceed(notifyRendered(objState, element, retries - 1)).delay(100.milliseconds)
      } else {
        val rendered = RenderedObject(objState, element, bbox)

        semaphore.withPermit {
          state.modify { s =>
            val existing = s.get(rendered.id)
            (existing.isEmpty, s + rendered)
          }.flatMap { isNew =>
            newObjects.publish(rendered).when(isNew) *> stateChanges.publish(rendered).unit
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

    def selectionBoundingBox =
      // We also update the bounding box whenever there's a state change (since that might have consequences)
      selectionRef.zipLatest(stateChanges).mapZIO { (ids, _) =>
        state.get.map { s => ids.toSeq.map(s.byId) }
      }.map { selectedObjects =>
        if (selectedObjects.isEmpty) None else Some(
          selectedObjects.tail.foldLeft(selectedObjects.head.boundingBox)(_ union _.boundingBox)
        )
      }

    def expandSelection(direction: Double => Boolean): UIO[Option[RenderedObject]] = for {
      selected <- selection.get
      objects <- state.get
    } yield {
      if (selected.isEmpty) objects.all.headOption else {
        val selectedObjects = selected.map(objects.byId).toSeq
        val origin = selectedObjects.tail.foldLeft(selectedObjects.head.boundingBox)(_ union _.boundingBox).middle

        val candidates = (objects.byId.keySet -- selected).map(objects.byId).filter { c =>
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
