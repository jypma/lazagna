package draw.client.render

import zio.Scope
import zio.lazagna.dom.Modifier
import zio.stream.ZPipeline

import draw.data.{ObjectState, ObjectStateBody}
import org.scalajs.dom

trait ObjectRenderer[T <: ObjectStateBody] {
  def render(initial: ObjectState[T]): Modifier[(dom.SVGElement,ZPipeline[Scope, Nothing, ObjectState[T], ObjectState[T]])]
}
