package zio.lazagna.dom.svg

import zio.lazagna.dom.{Element, Modifier}

import org.scalajs.dom
import scalajs.js

class SVGHelper(val svg: dom.svg.SVG) {
  val pt = svg.createSVGPoint()

  /** Converts client mouse coordinates into local SVG coordinates */
  def getClientPoint(event: dom.MouseEvent): dom.SVGPoint = {
    pt.x = event.clientX
    pt.y = event.clientY

    pt.matrixTransform(svg.getScreenCTM().inverse())
  }

  def triggerSVGDownload(): Unit = {
    val data = new dom.XMLSerializer().serializeToString(svg)
    val options = new dom.BlobPropertyBag {}
    options.`type` = "image/svg+xml;charset=utf-8"
    val blob = new dom.Blob(js.Array(data), options)
    val url = dom.URL.createObjectURL(blob)

    val a = dom.document.createElement("a").asInstanceOf[dom.HTMLElement]
    a.setAttribute("download", "exported.svg")
    a.setAttribute("href", url)
    a.setAttribute("target", "_blank")

    a.click()
  }
}

object SVGHelper {
  /** Binds a Helper instance if used as modifier directly under an SVG node */
  def apply(fn: SVGHelper => Modifier): Modifier = Element.thisElementAs { svgNode =>
    val h = new SVGHelper(svgNode.asInstanceOf[dom.svg.SVG])
    fn(h)
  }
}
