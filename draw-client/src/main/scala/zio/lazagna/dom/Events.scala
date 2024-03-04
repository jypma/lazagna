package zio.lazagna.dom

import scala.scalajs.js

import zio.stream.ZStream
import zio.{Chunk, Hub, Ref, Runtime, Scope, Unsafe, ZIO, ZLayer}

import org.scalajs.dom

import zio.lazagna.Filtered
import zio.lazagna.Filtered._
import zio.lazagna.Setup

/** Emits events from DOM objects, in a push-fashion. Operators have the same semantics as ZStream. */
case class EventsEmitter[-E <: dom.Event, +T](
  eventType: String,
  fn: E => ZIO[Scope & Setup, Filtered, T] = { (e:E) => ZIO.succeed(e) },
  overrideTarget: Option[dom.EventTarget] = None,
  others: Seq[EventsEmitter[_,T]] = Seq.empty
) {
  /** Presents the events emitter as a stream. This can be convenient when integrating with other ZIO
    * aspects. You'll typically have a fiber draining the stream. However, be aware that under scalaJS,
    * stopping fibers can be slow (about 1ms per fiber), so if you have 1000+ elements, try to avoid having a
    * stream (and fiber) per element.
    */
  def stream: ZStream[Scope with dom.EventTarget, Nothing, T] = {
    // TEST: Events are unregistered when the scope closes
    ZStream.unwrap {
      ZIO.service[Scope].map { scope =>
        ZStream.asyncZIO[Scope with dom.EventTarget, Nothing, T] { cb =>
          for {
            parent <- ZIO.service[dom.EventTarget]
            target = overrideTarget.getOrElse(parent)
            _ <- (ZIO.acquireRelease {
              ZIO.succeed {
                val listener: js.Function1[dom.Event, Unit] = { (event: dom.Event) =>
                  cb(Setup.start(fn(event.asInstanceOf[E])).map(o => Chunk(o)).catchAll {
                    case _:Filtered => ZIO.succeed(Chunk.empty)
                  })
                }
                target.addEventListener(eventType, listener)
                listener
              }
            } { listener =>
              ZIO.succeed {
                cb(ZIO.fail(None))
                target.removeEventListener(eventType, listener)
              }
            }).provideLayer(ZLayer.succeed(scope))
          } yield ()
        }
      }
    }
  }

  // Merge implementation is rather slow, but we'll only be merging a few similar event emitters together (hopefully).
  def merge[T1 >: T](other: EventsEmitter[_,T1]): EventsEmitter[E,T1] = copy(others = others :+ other)

  /** Makes the included event handlers receive events for scalajs.dom.window (instead of their actual parent) */
  def forWindow: EventsEmitter[E,T] = copy(overrideTarget = Some(dom.window))

  /** Makes the included event handlers receive events for scalajs.dom.document (instead of their actual parent) */
  def forDocument: EventsEmitter[E,T] = copy(overrideTarget = Some(dom.document))

  /** Performs the given operation on the events handled by this events emitter. */
  def apply[U](op: ZIO[Scope & Setup, Filtered,T] => ZIO[Scope & Setup, Filtered, U]): EventsEmitter[E,U] = copy(
    fn = e => op(fn(e)),
    others = others.map(_(op))
  )

  /** Returns a Modifier that runs this events emitter into the given hub when mounted */
  def -->[T1 >: T](target: Hub[T1]): Modifier = apply(_.flatMap(e => target.offer(e))).run

  /** Returns a Modifier that runs this events emitter into the given ref when mounted */
  def -->[T1 >: T](target: Ref[T1]): Modifier = apply(_.flatMap(e => target.set(e))).run

  /** Runs the side effects of this event emitter as a Modifier, binding to the parent where the modifier is mounted. */
  def run: Modifier = if (others.isEmpty) runThis else Modifier.combine(others.map(_.run) :+ runThis)

  private def runThis: Modifier = Modifier { parent =>
    val target = overrideTarget.getOrElse(parent)
    ZIO.scopeWith { scope =>
      (ZIO.acquireRelease(
        ZIO.succeed {
          val listener: js.Function1[dom.Event, Unit] = { (event: dom.Event) =>
            // TODO: Queue up events and pick them up as a Chunk while we're already running a callback
            Unsafe.unsafe { implicit unsafe =>
              Runtime.default.unsafe.runToFuture(Setup.start(fn(event.asInstanceOf[E])).provideLayer(ZLayer.succeed(scope)).catchAll {
                case _:Filtered => ZIO.unit
              })
            }
          }
          target.addEventListener(eventType, listener)
          listener
        })
        { (listener: js.Function1[dom.Event, Unit]) =>
          ZIO.succeed {
            target.removeEventListener(eventType, listener)
          }
        }).unit
    }
  }
}

object Events {
  implicit def emitterAsModifier[E <: dom.Event, T](emitter: EventsEmitter[E,T]): Modifier = emitter.run

  implicit class EventStreamOps[T](stream: ZStream[Scope with dom.EventTarget, Nothing, T]) {
    /** Converts the stream to a modifier that runs the stream in the background for its side effects only, providing
      * the parent to which the Modifier is mounted into the stream's environment. */
    def toModifier: Modifier = { parent =>
      stream.provideSomeLayer[Scope](ZLayer.succeed[dom.EventTarget](parent)).runDrain.forkScoped.unit
    }
  }

  implicit class baseEventsEmitterOps[E <: dom.Event](emitter: EventsEmitter[E,E]) {
    /** Maps the event to the event target's "value" property (assuming the target is a DOM element with a String
      * value property, e.g. input or textarea) */
    def asTargetValue: EventsEmitter[E, String] = emitter.apply(_.map(e => e.target match {
      case e:dom.HTMLInputElement => e.value
      case e:dom.HTMLTextAreaElement => e.value
      case _ => ""
    }))
  }

  private def event[E <: dom.Event](name: String):EventsEmitter[E,E] = EventsEmitter[E,E](name, others = Seq.empty)

  val onClick = event[dom.MouseEvent]("click")
  val onMouseDown = event[dom.MouseEvent]("mousedown")
  val onMouseUp = event[dom.MouseEvent]("mouseup")
  val onMouseMove = event[dom.MouseEvent]("mousemove")
  val onWheel = event[dom.WheelEvent]("wheel")
  val onKeyDown = event[dom.KeyboardEvent]("keydown")
  val onInput = event[dom.InputEvent]("input")
}
