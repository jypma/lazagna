package zio.lazagna.dom

import org.scalajs.dom
import scala.scalajs.js
import zio.Hub
import zio.ZIO
import zio.Scope
import zio.Unsafe

case class EventsEmitter[E <: dom.Event](eventType: String) {
  def -->(target: Hub[E]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      // Capture the actual JS function object, so we can properly remove it.
      val listener: js.Function1[dom.Event, Unit] = { (event: dom.Event) =>
        Unsafe.unsafe { implicit unsafe =>
          zio.Runtime.default.unsafe.run(
            ZIO.succeed {
              dom.console.log("Event " + eventType + " for ")
              dom.console.log(parent)
            } *>
              target.publish(event.asInstanceOf[E])
          ).getOrThrowFiberFailure()
        }
      }

      ZIO.acquireRelease {
        ZIO.succeed {
          dom.console.log("Adding " + eventType + " to")
          dom.console.log(parent)
          parent.addEventListener(eventType, listener)
        }
      } { _ =>
        ZIO.succeed {
          parent.removeEventListener(eventType, listener)
        }
      }
    }
  }
}

object Events {
  val onClick = EventsEmitter[dom.MouseEvent]("click")
}
