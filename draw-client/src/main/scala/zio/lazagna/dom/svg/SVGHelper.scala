package zio.lazagna.dom.svg

import zio.lazagna.dom.http.Request._
import zio.lazagna.dom.{Element, Modifier}

import draw.data.point.Point
import org.scalajs.dom

class SVGHelper(val svg: dom.svg.SVG) {
  val pt = svg.createSVGPoint()

  /** Converts client mouse coordinates into local SVG coordinates */
  def screenToLocal(event: dom.MouseEvent): dom.SVGPoint = {
    pt.x = event.clientX
    pt.y = event.clientY

    pt.matrixTransform(svg.getScreenCTM().inverse())
  }

  def localToScreen(point: Point): dom.SVGPoint = {
    pt.x = point.x;
    pt.y = point.y;
    pt.matrixTransform(svg.getScreenCTM())
  }
}

object SVGHelper {
  /** Binds a Helper instance if used as modifier directly under an SVG node */
  def apply(fn: SVGHelper => Modifier): Modifier = Element.thisElementAs { svgNode =>
    val h = new SVGHelper(svgNode.asInstanceOf[dom.svg.SVG])
    fn(h)
  }
}
