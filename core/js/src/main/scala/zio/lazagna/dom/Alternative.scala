package zio.lazagna.dom

import scala.scalajs.js
import scala.scalajs.js.timers.SetTimeoutHandle

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Modifier._
import zio.{Exit, Ref, Scope, ZIO, ZLayer}
import zio.lazagna.Setup

import org.scalajs.dom

object Alternative {
  /** Selects from a potentially unlimited lists of alternative renders, based on an element T. Whenever the
    * stream pushes a new T, the current render is fully replaced if T has changed.
    *
    * Renders of previous elements are discarded. */

  def mountOne[T](source: Consumeable[T])(render: PartialFunction[T, Modifier[Any]]): Modifier[Unit] = {
    case class State(t: T, scope: Scope.Closeable)

    for {
      current <- ZIO.acquireRelease(Ref.make[Option[State]](None))(_.get.flatMap(_.map(_.scope.close(Exit.unit)).getOrElse(ZIO.unit)))
      res <- Modifier { parent =>
        source.changes
          .map { t => (t, if (render.isDefinedAt(t)) render(t) else Modifier.empty) }
          .mapZIO { (t, rendered) =>
            for {
              state <- current.get
              start = System.currentTimeMillis()
              _ <- state.map(_.scope.close(Exit.unit)).getOrElse(ZIO.unit)
              clean = System.currentTimeMillis()
              newScope <- Scope.make
              // FIXME: Start new element in separate fiber, and interrupt if a new value comes in.
              _ <- Setup.start(rendered.provideSome[Setup](ZLayer.succeed(newScope), ZLayer.succeed(MountPoint(parent))))
              stop = System.currentTimeMillis()
              _ = if ((clean - start) > 1000 || (stop - clean) > 1000) { println(s"Closing took ${clean - start}ms, starting took ${stop - clean}ms") }
              _ <- current.set(Some(State(t, newScope)))
            } yield ()
          }
          .consume
      }
    } yield res
  }

  // TODO: Rethink viability of mountOneMemoized

  def option[T](source: Consumeable[Option[T]])(render: PartialFunction[T, Modifier[Any]]): Modifier[Unit] = {
    mountOne(source) {
      case Some(value) => render(value)
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
