package zio.lazagna.dom

import zio.{Scope, ZIO}

import org.scalajs.dom
import zio.ZLayer

trait Modifier {
  /** Returns a ZIO that applies this modifier to the visible DOM tree under the given parent and then returns. Any clean-up actions
    * that should be performed on unmount are tied to the given Scope. */
  def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit]
}

object Modifier {
  /** Returns a Modifier that combines all of the given modifiers to mount into the same parent when mounted. */
  implicit def combine(modifiers: Iterable[Modifier]): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.collectAll(modifiers.map(_.mount(parent))).unit
    }
  }

  def run(zio: ZIO[Scope & dom.Element, Nothing, Any]): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      zio.provideSomeLayer[Scope](ZLayer.succeed(parent)).unit
    }
  }

  /** Returns a Modifier that combines all of the given modifiers to mount into the same parent when mounted. */
  def combine(modifiers: Modifier*): Modifier = combine(modifiers.toSeq)
}
