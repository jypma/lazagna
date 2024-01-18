package zio.lazagna.dom

import scala.scalajs.js

import zio.stream.ZStream
import zio.{Chunk, Hub, Scope, Unsafe, ZIO}

import org.scalajs.dom
import zio.ZLayer

type EventsEmitter[T] = ZStream[Scope & dom.Element, Nothing, T]

object Events {
  private def event[E <: dom.Event](eventType: String): EventsEmitter[E] = {
    type JsEventHandler = js.Function1[dom.Event, Unit]

    ZStream.asyncZIO[Scope & dom.Element, Nothing, E] { cb =>
      for {
        parent <- ZIO.service[dom.Element]
        res <- ZIO.acquireRelease {
          ZIO.succeed {
            val listener: JsEventHandler = { (event: dom.Event) =>
              cb(ZIO.succeed(Chunk(event.asInstanceOf[E])))
            }
            parent.addEventListener(eventType, listener)
            listener
          }
        } { listener =>
          ZIO.succeed {
            parent.removeEventListener(eventType, listener)
          }
        }
      } yield res
    }
  }

  /** Returns a Modifier that runs the current EventsEmitter (and its transformations) for side effects */
  given emitter2modifier[E]: Conversion[EventsEmitter[E], Modifier] = eventsEmitter => new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      eventsEmitter.provideSomeLayer[Scope](ZLayer.succeed(parent)).runDrain.forkScoped.unit
    }
  }

  extension[E](eventsEmitter: EventsEmitter[E]) {
    /** Returns a Modifier that runs this events emitter into the given hub when mounted */
    def -->(target: Hub[E]): Modifier = eventsEmitter.mapZIO(e => target.offer(e))
  }

  val onClick = event[dom.MouseEvent]("click")
  val onMouseDown = event[dom.MouseEvent]("mousedown")
  val onMouseUp = event[dom.MouseEvent]("mouseup")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
}
