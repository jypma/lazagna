package zio.lazagna.dom

import org.scalajs.dom
import zio.ZIO
import zio.Scope
import zio.Hub
import zio.lazagna.Consumeable

object TextContent {
  def :=(value: String) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.succeed {
        parent.textContent = value
      }
    }
  }

  def <--[H](content: Consumeable[H,String]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      content(_.map { value =>
        parent.textContent = value
      }).consume
    }
  }
}
