package draw.client.render

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.stream.ZStream
import zio.{Hub, Ref, Semaphore, UIO, ZIO, ZLayer}

import draw.data.ObjectStateBody
import org.scalajs.dom

case class RenderedObject(id: String, state: ObjectStateBody, element: dom.Element)
trait RenderState {
  def initialObjectStates: Consumeable[RenderedObject]
  def objectState(id: String): Consumeable[RenderedObject]
  def notifyRendered(rendered: RenderedObject): UIO[Unit]

  /** Returns information about an object that might have been clicked to select it */
  def lookupForSelect(event: dom.MouseEvent): UIO[Option[RenderedObject]] = {
    getTargetObject(event, "selectTarget")
  }

  /** Returns information about an object that might have been clicked to edit it */
  def lookupForEdit(event: dom.MouseEvent): UIO[Option[RenderedObject]] = {
    getTargetObject(event, "editTarget")
  }

  protected def getTargetObject(event: dom.MouseEvent, className: String): UIO[Option[RenderedObject]]
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
  } yield new RenderState {
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

    def notifyRendered(rendered: RenderedObject) = semaphore.withPermit {
      state.modify { s =>
        val existing = s.get(rendered.id)
        (existing.isEmpty, s + rendered)
      }.flatMap { isNew =>
        newObjects.publish(rendered).when(isNew) *> stateChanges.publish(rendered).unit
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
  }

  val live = ZLayer.fromZIO(make)
}
