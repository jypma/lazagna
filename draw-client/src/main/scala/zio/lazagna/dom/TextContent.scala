package zio.lazagna.dom

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.{Scope, ZIO}

import org.scalajs.dom

object TextContent {
  def :=(value: String) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.succeed {
        parent.textContent = value
      }
    }
  }

  // TODO: Allow other types than String through implicits
  def <--(content: Consumeable[String]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      content.map { value =>
        parent.textContent = value
      }.consume
    }
  }
}
