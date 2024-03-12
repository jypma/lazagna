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
}
object RenderState {
  def make = for {
    state <- Ref.make[Map[String,RenderedObject]](Map.empty)
    semaphore <- Semaphore.make(1)
    stateChanges <- Hub.bounded[RenderedObject](16)
    newObjects <- Hub.bounded[RenderedObject](16)
  } yield new RenderState {
    def initialObjectStates = ZStream.unwrapScoped {
      semaphore.withPermit {
        state.get.map(_.values.toSeq).flatMap { initial =>
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
      state.modify { map =>
        val existing = map.get(rendered.id)
        (existing.isEmpty, map + (rendered.id -> rendered))
      }.flatMap { isNew =>
        newObjects.publish(rendered).when(isNew) *> stateChanges.publish(rendered).unit
      }
    }
  }

  val live = ZLayer.fromZIO(make)
}
