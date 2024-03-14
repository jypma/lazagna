package zio.lazagna.dom

import zio.{Scope, ZIO, ZLayer}

import org.scalajs.dom

/** A modifier is a ZIO that applies certain changes to its mount point, optionally returning a value T (which
  * may be Unit if the Modifier only executes side effects). */
type Modifier[+T] = ZIO[Modifier.MountPoint & Scope, Nothing, T]

object Modifier {
  case class MountPoint(parent: dom.Element, after: Option[dom.Element] = None)

  def apply[T](fn: dom.Element => Modifier[T]): Modifier[T] = ZIO.service[MountPoint].flatMap { i => fn(i.parent) }

  /** Returns a Modifier that combines all of the given modifiers to mount into the same parent when mounted. */
  implicit def combine[T](modifiers: Iterable[Modifier[T]]): Modifier[Iterable[T]] =
    ZIO.collectAll(modifiers)

  /** Returns a Modifier that combines all of the given modifiers to mount into the same parent when mounted. */
  def combine[T](modifiers: Modifier[T]*): Modifier[Iterable[T]] = combine(modifiers.toSeq)

  /** Returns a Modifier that combines all of the given modifiers to mount into the same parent when mounted, discarding the result. */
  def all[T](modifiers: Modifier[T]*): Modifier[Unit] = combine(modifiers.toSeq).unit

  implicit class ModifierOps[T](modifier: Modifier[T]) {
    def mount(parent: dom.Element): ZIO[Scope, Nothing, T] =
      modifier.provideSome[Scope](ZLayer.succeed(Modifier.MountPoint(parent)))
  }

  /** A Modifier that does nothing. */
  val empty:Modifier[Unit] = ZIO.unit
}
