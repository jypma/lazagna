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

case class EventsEmitter[E <: dom.Event,T](private val eventType: String, private val transform: ZStream[Scope, Nothing, E] => ZStream[Scope, Nothing, T]) {
  type JsEventHandler = js.Function1[dom.Event, Unit]

  def apply[U](t: ZStream[Scope, Nothing, T] => ZStream[Scope, Nothing, U]) = copy(transform = transform.andThen(t))

  def -->(target: Hub[T]) = new Modifier {
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

      transform(stream).mapZIO(e => target.offer(e)).runDrain.forkScoped.unit
    }
  }
}

object Events {
  private def event[E <: dom.Event](eventType: String) = EventsEmitter[E,E](eventType, s => s)

  val onClick = event[dom.MouseEvent]("click")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
}
