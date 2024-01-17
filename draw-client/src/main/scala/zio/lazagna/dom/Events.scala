package zio.lazagna.dom

import scala.scalajs.js

import zio.stream.ZStream
import zio.{Chunk, Hub, Scope, Unsafe, ZIO}

import org.scalajs.dom

/** Emits a certain kind of event, and can create Modifier instances that run those events into a Hub or execute side effects. */
trait EventsEmitter[T] {
  private[lazagna] def stream(parent: dom.Element): ZStream[Scope, Nothing, T]

  /** Returns a new EventsEmitter that performs the given arbitrary transformation on the event stream */
  def apply[U](t: ZStream[Scope, Nothing, T] => ZStream[Scope, Nothing, U]): EventsEmitter[U]

  /** Returns a Modifier that runs this events emitter into the given hub when mounted */
  def -->(target: Hub[T]) = this.apply(_.mapZIO(e => target.offer(e))).run

  /** Returns a Modifier that runs the current EventsEmitter (and its transformations) for side effects */
  def run = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      stream(parent).runDrain.forkScoped.unit
    }
  }

  /** Returns a new Consumeable that emits values to its stream when either this or the other emit. */
  def merge[T1 <: T](other: EventsEmitter[T1]) = MergedEventsEmitter(this, other, s => s)
}

case class SingleEventsEmitter[E <: dom.Event,T](private val eventType: String, private val transform: ZStream[Scope, Nothing, E] => ZStream[Scope, Nothing, T]) extends EventsEmitter[T] {
  type JsEventHandler = js.Function1[dom.Event, Unit]

  override def apply[U](t: ZStream[Scope, Nothing, T] => ZStream[Scope, Nothing, U]) = copy(transform = transform.andThen(t))

  override def stream(parent: dom.Element) = transform(ZStream.asyncZIO[Scope, Nothing, E] { cb =>
    ZIO.acquireRelease {
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
  })
}

case class MergedEventsEmitter[E1,E2 <: E1,T](a: EventsEmitter[E1], b: EventsEmitter[E2], transform: ZStream[Scope,Nothing,E1] => ZStream[Scope, Nothing, T]) extends EventsEmitter[T] {
  override def apply[U](t: ZStream[Scope, Nothing, T] => ZStream[Scope, Nothing, U]) = copy(transform = transform.andThen(t))

  override def stream(parent: dom.Element) = transform(a.stream(parent).merge(b.stream(parent)))
}

object Events {
  private def event[E <: dom.Event](eventType: String) = SingleEventsEmitter[E,E](eventType, s => s)

  val onClick = event[dom.MouseEvent]("click")
  val onMouseDown = event[dom.MouseEvent]("mousedown")
  val onMouseUp = event[dom.MouseEvent]("mouseup")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
}
