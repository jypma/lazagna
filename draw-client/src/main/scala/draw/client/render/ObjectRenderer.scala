package draw.client.render

import zio.lazagna.Consumeable
import zio.lazagna.dom.Modifier

import draw.data.{ObjectState, ObjectStateBody}

trait ObjectRenderer[T <: ObjectStateBody] {
  def render(initial: ObjectState[T], furtherEvents: Consumeable[T]): Modifier
}
