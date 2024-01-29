package zio.lazagna.dom

import scala.scalajs.js

import zio.stream.ZStream
import zio.{Chunk, Hub, Scope, Unsafe, ZIO, ZLayer}

import org.scalajs.dom
import zio.Ref

type EventsEmitter[T] = ZStream[Scope with dom.Element, Nothing, T]

object Events {
  private def event[E <: dom.Event](eventType: String): EventsEmitter[E] = {
    type JsEventHandler = js.Function1[dom.Event, Unit]

    ZStream.asyncScoped[Scope with dom.Element, Nothing, E] { cb =>
      for {
        parent <- ZIO.service[dom.Element]
        _ <- ZIO.acquireRelease {
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
      } yield ()
    }
  }

  /** Returns a Modifier that runs the current EventsEmitter (and its transformations) for side effects */
  implicit def emitter2modifier[E](eventsEmitter: EventsEmitter[E]): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      eventsEmitter.provideSomeLayer[Scope](ZLayer.succeed(parent)).runDrain.forkScoped.unit
    }
  }

  implicit class EventsEmitterOps[E](eventsEmitter: EventsEmitter[E]) {
    /** Returns a Modifier that runs this events emitter into the given hub when mounted */
    def -->(target: Hub[E]): Modifier = eventsEmitter.mapZIO(e => target.offer(e))

    /** Returns a Modifier that runs this events emitter into the given ref when mounted */
    def -->(target: Ref[E]): Modifier = eventsEmitter.mapZIO(e => target.set(e))

  }

  val onClick = event[dom.MouseEvent]("click")
  val onMouseDown = event[dom.MouseEvent]("mousedown")
  val onMouseUp = event[dom.MouseEvent]("mouseup")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
  val onWheel = event[dom.WheelEvent]("wheel")
}
