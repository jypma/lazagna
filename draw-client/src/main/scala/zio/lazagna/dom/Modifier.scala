package zio.lazagna.dom

import org.scalajs.dom
import zio.ZIO
import zio.Scope

trait Modifier {
  /** Returns a ZIO that applies this modifier to the visible DOM tree under the given parent and then returns. Any clean-up actions
    * that should be performed on unmount are tied to the given Scope. */
  def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit]
}
