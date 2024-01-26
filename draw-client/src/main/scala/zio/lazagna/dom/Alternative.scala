package zio.lazagna.dom

import zio.stream.{ZPipeline, ZStream}

import org.scalajs.dom
import scala.scalajs.js

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Modifier._
import scala.scalajs.js.timers.SetTimeoutHandle

object Alternative {
  /** Selects from a potentially unlimited lists of alternative renders, based on an element T. Whenever the stream
    * pushes a new T, the current render is fully replaced if T has changed. */
  def mountOne[T,E <: dom.Element](render: T => Element[E]): ZPipeline[Any, Nothing, T, Children.ChildOp] = {
    case class State(current: T, rendered: Element[E])

    ZPipeline.mapAccum(None: Option[State]) { (state: Option[State], in: T) =>
      state match {
        case Some(state) if state.current == in =>
          (Some(state), Seq.empty)
        case Some(state) =>
          val rendered = render(in)
          (Some(State(in, rendered)), Seq(Children.Delete(state.rendered), Children.Append(rendered)))
        case _ =>
          val rendered = render(in)
          (Some(State(in, rendered)), Seq(Children.Append(rendered)))
      }
    }.mapStream { ops => ZStream.fromIterable(ops) }
  }

  /** Selects from a limited set of alternative renders. All renders are always mounted, but hidden using CSS.
    * Whenever the consumable pushes a new T, the matching alternative is shown. If no alternative matches,
    * none are shown. */
  def showOne[T,E <: dom.Element](alternatives: Map[T, Element[E]], source: Consumeable[T], initial: Option[T] = None) = {
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
          } else {
          }
        }
      }.consume),
    )
  }
}
