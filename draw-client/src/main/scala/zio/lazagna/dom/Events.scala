package zio.lazagna.dom

import org.scalajs.dom
import scala.scalajs.js
import zio.Hub
import zio.ZIO
import zio.Scope
import zio.Unsafe
import zio.stream.ZStream
import zio.Ref
import zio.Chunk

/** Emits a certain kind of event, and can create Modifier instances that run those events into a Hub or execute side effects. */
case class EventsEmitter[E <: dom.Event,T](private val eventType: String, private val transform: ZStream[Scope, Nothing, E] => ZStream[Scope, Nothing, T]) {
  type JsEventHandler = js.Function1[dom.Event, Unit]

  /** Returns a new EventsEmitter that performs the given arbitrary transformation on the event stream */
  def apply[U](t: ZStream[Scope, Nothing, T] => ZStream[Scope, Nothing, U]) = copy(transform = transform.andThen(t))

  /** Returns a Modifier that runs this events emitter into the given hub when mounted */
  def -->(target: Hub[T]) = this.apply(_.mapZIO(e => target.offer(e))).run

  /** Returns a Modifier that runs the current EventsEmitter (and its transformations) for side effects */
  def run = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {

      val  stream = ZStream.asyncZIO[Scope, Nothing, E] { cb =>
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
      }

      transform(stream).runDrain.forkScoped.unit
    }
  }
}

object Events {
  private def event[E <: dom.Event](eventType: String) = EventsEmitter[E,E](eventType, s => s)

  val onClick = event[dom.MouseEvent]("click")
  val onMouseDown = event[dom.MouseEvent]("mousedown")
  val onMouseUp = event[dom.MouseEvent]("mouseup")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
}
