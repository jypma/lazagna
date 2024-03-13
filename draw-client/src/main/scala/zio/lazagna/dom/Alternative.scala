package zio.lazagna.dom

import scala.scalajs.js
import scala.scalajs.js.timers.SetTimeoutHandle

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Modifier._
import zio.{Exit, Ref, Scope, ZIO}

import org.scalajs.dom
import zio.ZLayer

object Alternative {
  /** Selects from a potentially unlimited lists of alternative renders, based on an element T. Whenever the
    * stream pushes a new T, the current render is fully replaced if T has changed.
    *
    * Renders of previous elements are discarded. */

  // TEST: Close current scope when unmounted
  def mountOne[T](source: Consumeable[T])(render: T => Modifier[Any]): Modifier[Unit] = {
    case class State(t: T, scope: Scope.Closeable)

    for {
      current <- ZIO.acquireRelease(Ref.make[Option[State]](None))(_.get.flatMap(_.map(_.scope.close(Exit.unit)).getOrElse(ZIO.unit)))
      res <- Modifier { parent =>
        source.changes
          .map { t => (t, render(t)) }
          .mapZIO { (t, rendered) =>
            for {
              state <- current.get
              _ <- state.map(_.scope.close(Exit.unit)).getOrElse(ZIO.unit)
              newScope <- Scope.make
              _ <- rendered.provide(ZLayer.succeed(newScope), ZLayer.succeed(MountPoint(parent)))
              _ <- current.set(Some(State(t, newScope)))
            } yield ()
          }
          .consume
      }
    } yield res
  }

  // TODO: Rethink viability of mountOneMemoized

  def option[T](source: Consumeable[Option[T]])(render: T => Modifier[Any]): Modifier[Unit] = {
    mountOne(source) {
      case Some(value) => render(value)
      case None => Modifier.empty
    }
  }

  /** Selects from a limited set of alternative renders. All renders are always mounted, but hidden using CSS.
    * Whenever the consumable pushes a new T, the matching alternative is shown. If no alternative matches,
    * none are shown. */
  def showOne[T](source: Consumeable[T], alternatives: Map[T, Element[_ <: dom.Element]], initial: Option[T] = None): Modifier[dom.HTMLElement] = {
    def clsOf(name: String): String = s"alternative showOne ${name}"
    def cls(active: Boolean): String = clsOf(if (active) "active" else "inactive")

    var timeouts = Map.empty[T, SetTimeoutHandle]

    for {
      state <- Ref.make[Map[T, dom.Element]](Map.empty)
      res <- div(
        `class` := "alternatives",
        alternatives.map { (key, child) =>
          div(
            `class` := cls(initial.contains(key)),
            child
          ).tap { target => state.update(_ + (key -> target)) }
        },
        source.changes.mapZIO(t => state.get.map((_, t))). map { (targets, t) =>
          alternatives.foreach { (key, element) =>
            val divElmt = targets(key)
            divElmt.setAttribute("class", cls(t == key))
            timeouts.get(key).foreach(js.timers.clearTimeout)
            if (t != key) {
              timeouts += key -> js.timers.setTimeout(1000) {
                divElmt.setAttribute("class", clsOf("hidden"))
              }
            }
          }
        }.consume
      )
    } yield res
  }
}
