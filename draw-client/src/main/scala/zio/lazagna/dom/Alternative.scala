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

object Alternative {
  /** Selects from a potentially unlimited lists of alternative renders, based on an element T. Whenever the stream
    * pushes a new T, the current render is fully replaced if T has changed. */
  def mountOne[T](source: Consumeable[T])(render: T => Modifier): Modifier = {
    case class State(t: T, scope: Scope.Closeable)

    Modifier.unwrap{
      for {
        current <- Ref.make[Option[State]](None)
      } yield new Modifier {
        override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
          source.changes
            .map { t => (t, render(t)) }
            .mapZIO { (t, rendered) =>
              for {
                state <- current.get
                _ <- state.map(_.scope.close(Exit.unit)).getOrElse(ZIO.unit)
                newScope <- Scope.make
                _ <- newScope.extend(rendered.mount(parent))
                _ <- current.set(Some(State(t, newScope)))
              } yield ()
            }
            .consume
        }
      }
    }
  }

  /** Selects from a limited set of alternative renders. All renders are always mounted, but hidden using CSS.
    * Whenever the consumable pushes a new T, the matching alternative is shown. If no alternative matches,
    * none are shown. */
  def showOne[T](source: Consumeable[T], alternatives: Map[T, Element[_]], initial: Option[T] = None): Modifier = {
    def clsOf(name: String): String = s"alternative showOne ${name}"
    def cls(active: Boolean): String = clsOf(if (active) "active" else "inactive")

    var timeouts = Map.empty[T, SetTimeoutHandle]

    div(
      `class` := "alternatives",
      alternatives.map { (key, child) =>
        div(
          `class` := cls(initial.contains(key)),
          child
        )
      },
      run(source.changes.map { t =>
        alternatives.foreach { (key, element) =>
          val parent = element.target.parentNode.asInstanceOf[dom.Element]
          parent.setAttribute("class", cls(t == key))
          timeouts.get(key).foreach(js.timers.clearTimeout)
          if (t != key) {
            timeouts += key -> js.timers.setTimeout(1000) {
              parent.setAttribute("class", clsOf("hidden"))
            }
          }
        }
      }.consume),
    )
  }
}
