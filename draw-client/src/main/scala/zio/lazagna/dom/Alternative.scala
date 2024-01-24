package zio.lazagna.dom

import zio.stream.{ZPipeline, ZStream}

import org.scalajs.dom

object Alternative {
  def alternative[T,E <: dom.Element](render: T => Element[E]): ZPipeline[Any, Nothing, T, Children.ChildOp] = {
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
}
