package zio.lazagna.dom.svg

import zio.lazagna.dom.http.Request._
import zio.lazagna.dom.{Element, Modifier}

import draw.geom.{Point, Rectangle}
import org.scalajs.dom

class SVGHelper(val svg: dom.svg.SVG) {
  import SVGHelper._

  val pt = svg.createSVGPoint()

  /** Returns the bounding box of the given element, in SVG coordinates */
  def svgBoundingBox(element: dom.SVGLocatable, margin: Int = 0): Rectangle = {
    val elementToScreen = element.getScreenCTM()
    val screenToSVG = svg.getScreenCTM().inverse()
    val matrix = screenToSVG.multiply(elementToScreen)

    val bbox = element.getBBox().toRectangle.expand(margin)
    pt.x = bbox.origin.x
    pt.y = bbox.origin.y
    val p1 = pt.matrixTransform(matrix)

    pt.x = bbox.span.x
    pt.y = bbox.span.y
    val p2 = pt.matrixTransform(matrix)

    Rectangle(p1.toPoint, p2.toPoint)
  }

  /** Converts client mouse coordinates into SVG coordinates */
  def screenToSvg(event: dom.MouseEvent): dom.SVGPoint = {
    pt.x = event.clientX
    pt.y = event.clientY

    pt.matrixTransform(svg.getScreenCTM().inverse())
  }

  /** Converts SVG coordinates to screen coordinates */
  def svgToScreen(point: Point): dom.SVGPoint = {
    pt.x = point.x;
    pt.y = point.y;
    pt.matrixTransform(svg.getScreenCTM())
  }
}

object SVGHelper {
  /** Binds a Helper instance if used as modifier directly under an SVG node */
  def apply[T](fn: SVGHelper => Modifier[T]): Modifier[T] = Element.thisElementAs { svgNode =>
    val h = new SVGHelper(svgNode.asInstanceOf[dom.svg.SVG])
    fn(h)
  }

  implicit class SVGRectOps(rect: dom.SVGRect) {
    def toRectangle = Rectangle(Point(rect.x, rect.y), rect.width, rect.height)
  }

  implicit class SVGPointOps(p: dom.SVGPoint) {
    def toPoint = Point(p.x, p.y)
  }
}
