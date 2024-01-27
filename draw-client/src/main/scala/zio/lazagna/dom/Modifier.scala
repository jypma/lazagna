package zio.lazagna.dom

import zio.{Scope, ZIO}

import org.scalajs.dom
import zio.ZLayer

// TODO: Investigate replacing with type alias to ZIO[dom.Element & Scope, Nothing, Unit]. That might be too weak though.
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

  /** Runs the given ZIO when this modifier is mounted. It can optionally access the parent into which it was mounted in its environment. */
  def run(zio: ZIO[Scope & dom.Element, Nothing, Any]): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      zio.provideSomeLayer[Scope](ZLayer.succeed(parent)).unit
    }
  }

  /** Runs the given ZIO to extract a modifier, and mounts it */
  def unwrap(zio: ZIO[Scope, Nothing, Modifier]): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      zio.flatMap(_.mount(parent))
    }
  }

  /** Returns a Modifier that combines all of the given modifiers to mount into the same parent when mounted. */
  def combine(modifiers: Modifier*): Modifier = combine(modifiers.toSeq)
}
