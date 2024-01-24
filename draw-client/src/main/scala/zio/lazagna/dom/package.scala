package zio.lazagna

import org.scalajs.dom
import zio.stream.ZStream
import zio.Scope


package object dom {
  /** An EventsEmitter is a stream that emits DOM events for a given parent (which is taken from the stream's environment when starting). */
  type EventsEmitter[T] = ZStream[Scope with dom.Element, Nothing, T]

}
