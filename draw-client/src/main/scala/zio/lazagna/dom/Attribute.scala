package zio.lazagna.dom

import org.scalajs.dom
import zio.ZIO
import zio.Scope

case class Attribute(name: String) {
  def :=(value: String) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.succeed {
        dom.console.log("set " + name + " to " + value)
        parent.setAttribute(name, value)
      }
    }
  }
}

object Attribute {
  val title = Attribute("title")
}
