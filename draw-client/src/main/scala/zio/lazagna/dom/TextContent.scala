package zio.lazagna.dom

import zio.lazagna.ZIOOps._
import org.scalajs.dom
import zio.ZIO
import zio.Scope
import zio.Hub


object TextContent {
  def :=(value: String) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.succeed {
        parent.textContent = value
      }
    }
  }

  def <--(content: Hub[String]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      consume(content) { value =>
        ZIO.succeed {
          parent.textContent = value
        }
      }
    }
  }
}
