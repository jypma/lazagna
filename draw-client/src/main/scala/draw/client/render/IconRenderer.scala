package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.{textContent, _}
import zio.lazagna.dom.Modifier

import draw.data.{IconState, ObjectState}

import DrawingRenderer.dataX
import DrawingRenderer.dataY
import DrawingRenderer.iconSize

object IconRenderer {
  def make = ZIO.succeed {
    new ObjectRenderer[IconState] {
      override def render(initial: ObjectState[IconState], furtherEvents: Consumeable[IconState]): Modifier = {
        val position = furtherEvents
          .collect { case IconState(pos, _, _) => pos }
          .changes

        g(
          id := s"icon${initial.id}",
          cls := "icon editTarget",
          transform <-- position.map(p => s"translate(${p.x},${p.y})"),
          dataX <-- position.map(_.x),
          dataY <-- position.map(_.y),
          g(
            cls := "selectTarget",
            use(
              svgTitle(textContent := initial.body.symbol.name),
              href := initial.body.symbol.href,
              cls := "icon",
              width := iconSize,
              height := iconSize,
              x := -iconSize / 2,
              y := -iconSize / 2
            ),
          ),
          text(
            cls := "label",
            x := 0,
            y := iconSize / 2,
            textContent <-- furtherEvents
              .collect { case IconState(_,_,label) => label }
          )
        )
      }
    }
  }
}
