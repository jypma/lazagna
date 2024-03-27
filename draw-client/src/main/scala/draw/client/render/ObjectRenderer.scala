package draw.client.render

import zio.lazagna.dom.Modifier

import draw.data.{ObjectState, ObjectStateBody}
import org.scalajs.dom
import zio.stream.ZPipeline
import zio.Scope

trait ObjectRenderer[T <: ObjectStateBody] {
  def render(initial: ObjectState[T]): Modifier[(dom.SVGElement,ZPipeline[Scope, Nothing, ObjectState[T], ObjectState[T]])]
}
