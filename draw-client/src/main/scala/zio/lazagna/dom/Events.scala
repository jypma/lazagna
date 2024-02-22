package zio.lazagna.dom

import scala.scalajs.js

import zio.stream.ZStream
import zio.{Chunk, Hub, Ref, Scope, Unsafe, ZIO, ZLayer}

import org.scalajs.dom

type EventsEmitter[T] = ZStream[Scope with dom.EventTarget, Nothing, T]

object Events {
  // TODO: See if we can do the event listening with just one fiber per scope, delegating into running handlers where necessary?
  // TEST: Events are unregistered when the scope closes
  private def event[E <: dom.Event](eventType: String): EventsEmitter[E] = {
    type JsEventHandler = js.Function1[dom.Event, Unit]

    // Not asyncScoped, since we want the scope to control the stream, not the other way around.
    ZStream.unwrap {
      ZIO.service[Scope].map { scope =>
        ZStream.asyncZIO[Scope with dom.EventTarget, Nothing, E] { cb =>
          for {
            parent <- ZIO.service[dom.EventTarget]
            _ <- (ZIO.acquireRelease {
              ZIO.succeed {
                val listener: JsEventHandler = { (event: dom.Event) =>
                  cb(ZIO.succeed(Chunk(event.asInstanceOf[E])))
                }
                parent.addEventListener(eventType, listener)
                listener
              }
            } { listener =>
              ZIO.succeed {
                cb(ZIO.fail(None))
                parent.removeEventListener(eventType, listener)
              }
            }).provideLayer(ZLayer.succeed(scope))
          } yield ()
        }
      }
    }
  }

  /** Returns a Modifier that runs the current EventsEmitter (and its transformations) for side effects */
  implicit class EmitterAsModifier[E](eventsEmitter: EventsEmitter[E]) extends Modifier {
    def mountEvents(parent: dom.EventTarget): ZIO[Scope, Nothing, Unit] = {
      // forkScoped is slow here (stopping 1000 fibers from scope takes a long time).
      // Instead, we only fork, and rely on the scope stopping causing the callback to stop the stream (and fiber) instead.
      eventsEmitter.provideSomeLayer[Scope](ZLayer.succeed(parent)).runDrain.fork.unit
    }

    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      mountEvents(parent)
    }
  }

  implicit class EventsEmitterOps[E](eventsEmitter: EventsEmitter[E]) {
    /** Returns a Modifier that runs this events emitter into the given hub when mounted */
    def -->(target: Hub[E]): Modifier = eventsEmitter.mapZIO(e => target.offer(e))

    /** Returns a Modifier that runs this events emitter into the given ref when mounted */
    def -->(target: Ref[E]): Modifier = eventsEmitter.mapZIO(e => target.set(e))
  }

  implicit class EventsEmitterTargetOps[E <: dom.Event](eventsEmitter: EventsEmitter[E]) {
    /** Maps the event to the event target's "value" property (assuming the target is a HTMLInputElement) */
    def asTargetValue: EventsEmitter[String] = eventsEmitter.map(e => e.target match {
      case e:dom.HTMLInputElement => e.value
      case _ => ""
    })
  }

  /** Makes the included event handlers receive events for scalajs.dom.window (instead of their actual parent) */
  def windowEvents(handlers: Modifier*): Modifier = rerouteEvents(dom.window, handlers)

  /** Makes the included event handlers receive events for scalajs.dom.document (instead of their actual parent) */
  def documentEvents(handlers: Modifier*): Modifier = rerouteEvents(dom.document, handlers)

  private def rerouteEvents(target: dom.EventTarget, handlers: Seq[Modifier]): Modifier = {
    Modifier.combine(handlers.map {
      case m:EmitterAsModifier[_] => new Modifier {
        override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
          m.mountEvents(target)
        }
      }

      case other => other
    })
  }

  val onClick = event[dom.MouseEvent]("click")
  val onMouseDown = event[dom.MouseEvent]("mousedown")
  val onMouseUp = event[dom.MouseEvent]("mouseup")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
  val onWheel = event[dom.WheelEvent]("wheel")
  val onKeyDown = event[dom.KeyboardEvent]("keydown")
  val onInput = event[dom.InputEvent]("input")
}
