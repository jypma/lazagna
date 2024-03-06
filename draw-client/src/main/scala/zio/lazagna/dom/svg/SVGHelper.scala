package zio.lazagna.dom.svg

import zio.lazagna.dom.http.Request._
import zio.lazagna.dom.{Element, Modifier}

import draw.data.point.Point
import org.scalajs.dom

class SVGHelper(val svg: dom.svg.SVG) {
  val pt = svg.createSVGPoint()

  def svgBBox(element: dom.SVGGElement, margin: Int = 0): dom.SVGRect = {
    val bbox = element.getBBox();
    bbox.x -= margin
    bbox.y -= margin
    bbox.width += 2 * margin
    bbox.height += 2 * margin
    pt.x = bbox.x
    pt.y = bbox.y
    val elementToScreen = element.getScreenCTM()
    val screenToSVG = svg.getScreenCTM().inverse()
    val matrix = screenToSVG.multiply(elementToScreen) // perhaps the other way round?

    val p1 = pt.matrixTransform(matrix)

    pt.x = bbox.x + bbox.width
    pt.y = bbox.y + bbox.height
    val p2 = pt.matrixTransform(matrix)

    bbox.x = p1.x
    bbox.y = p1.y
    bbox.width = (p2.x - p1.x)
    bbox.height = (p2.y - p1.y)
    bbox
  }

  /** Converts client mouse coordinates into local SVG coordinates */
  def screenToSvg(event: dom.MouseEvent): dom.SVGPoint = {
    pt.x = event.clientX
    pt.y = event.clientY

    pt.matrixTransform(svg.getScreenCTM().inverse())
  }

  def svgToScreen(point: Point): dom.SVGPoint = {
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
