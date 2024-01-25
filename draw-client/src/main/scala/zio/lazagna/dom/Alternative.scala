package zio.lazagna.dom

import zio.stream.{ZPipeline, ZStream}

import org.scalajs.dom
import zio.lazagna.Consumeable
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Attribute._

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
  def showOne[T,E <: dom.Element](alternatives: Map[T, Element[E]], source: Consumeable[T]) = {
    div(
      alternatives.map { (key, child) =>
        div(
          style <-- source.map { t =>
            if (t == key) "display: initial" else "display: none"
          },
          child
        )
      },
    )
  }
}
