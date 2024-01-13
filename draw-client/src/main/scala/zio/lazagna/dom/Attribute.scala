package zio.lazagna.dom

import org.scalajs.dom
import zio.ZIO
import zio.Scope

case class Attribute(name: String) {
  def :=(value: Double): Modifier = :=(value.toString)

  def :=(value: Int): Modifier = :=(value.toString)

  def :=(value: String): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.succeed {
        parent.setAttribute(name, value)
      }
    }
  }
}

object Attribute {
  val title = Attribute("title")
  val width = Attribute("width")
  val height = Attribute("height")

  // SVG attributes, keeping in shared scope for now
  val fill = Attribute("fill")
  val cx = Attribute("cx")
  val cy = Attribute("cy")
  val r = Attribute("r")
}
