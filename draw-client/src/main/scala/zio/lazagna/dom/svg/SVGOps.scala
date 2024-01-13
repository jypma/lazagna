package zio.lazagna.dom.svg

import org.scalajs.dom

import zio.lazagna.dom.Modifier
import zio.lazagna.dom.Element

object SVGOps {
  /** Utility class that converts client mouse coordinates into local SVG coordinates */
  case class CoordinateHelper(svg: dom.svg.SVG) {
    val pt = svg.createSVGPoint()

    def getClientPoint(event: dom.MouseEvent): dom.SVGPoint = {
      pt.x = event.clientX
      pt.y = event.clientY

      pt.matrixTransform(svg.getScreenCTM().inverse())
    }
  }

  /** Binds a CoordinateHelper instance if used as modifier directly under an SVG node */
  def coordinateHelper(fn: CoordinateHelper => Modifier): Modifier = Element.thisElementAs { svgNode =>
    val h = CoordinateHelper(svgNode.asInstanceOf[dom.svg.SVG])
    fn(h)
  }
}
