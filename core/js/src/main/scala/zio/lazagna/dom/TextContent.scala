package zio.lazagna.dom

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._

object TextContent {
  def :=(value: String) = Modifier { parent =>
    ZIO.succeed {
      parent.textContent = value
    }
  }

  // TODO: Allow other types than String through implicits
  def <--(content: Consumeable[String]) = Modifier { parent =>
    content.map { value =>
      parent.textContent = value
    }.consume
  }
}
