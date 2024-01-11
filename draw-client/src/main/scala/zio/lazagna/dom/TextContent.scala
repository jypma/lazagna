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
        dom.console.log("set to " + value)
        parent.textContent = value
      }
    }
  }

  // TODO: Investigate a nicer way to allow .map() on incoming hub messages.
  // Perhaps move away from <--, and instead just
  /*
   div(
     textContent(myHub)(_.map(_.toString))
   )
   */
  // Or change the datatype to ZStream[Scope, Nothing, Unit] and provide an implicit conversion from Hub
  def <--(content: Hub[String]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      consume(content) { value =>
        dom.console.log("Setting to " + value)
        ZIO.succeed {
          dom.console.log(parent)
          parent.textContent = value
        }
      }
    }
  }
}
