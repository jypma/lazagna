package zio.lazagna.dom

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.stream.{ZPipeline, ZStream}
import zio.{Chunk, Scope, ZIO}

import org.scalajs.dom

case class Attribute(name: String) {
  import Attribute._

  def :=[T](value: T)(implicit ev: AttributeType[T]): Modifier[Unit] = set(value)

  def set[T](value: T)(implicit ev: AttributeType[T]): Modifier[Unit] = Modifier { parent =>
    ZIO.succeed {
      ev.setTo(parent, name, value)
      // FIXME: Consider removing the attribute when scope ends, if it wasn't set before
    }
  }

  def <--[T](content: Consumeable[T])(implicit ev: AttributeType[T]) = Modifier { parent =>
    content.map { value =>
      ev.setTo(parent, name, value)
    }.consume
  }
}

object Attribute {
  /** Marker trait that indicates values that can be handled directly on an attribute  */
  sealed trait AttributeType[T] {
    def setTo(parent: dom.Element, name: String, value: T): Unit
  }
  private def toStringAttribute[T] = new AttributeType[T] {
    override def setTo(parent: dom.Element, name: String, value: T): Unit = {
      parent.setAttribute(name, value.toString)
    }
  }
  object AttributeType {
    implicit val string: AttributeType[String] = toStringAttribute
    implicit val int: AttributeType[Int] = toStringAttribute
    implicit val long: AttributeType[Long] = toStringAttribute
    implicit val float: AttributeType[Float] = toStringAttribute
    implicit val double: AttributeType[Double] = toStringAttribute
    implicit val boolean: AttributeType[Boolean] = new AttributeType[Boolean] {
      override def setTo(parent: dom.Element, name: String, value: Boolean): Unit = {
        (name, parent) match {
          // TODO: See if there's a nicer, more generic way to do this
          case ("checked", e:dom.HTMLInputElement) =>
            e.checked = value
          case _ =>
            if (value) {
              parent.setAttribute(name, "true")
            } else {
              parent.removeAttribute(name)
            }
        }
      }
    }
  }

  /** Combines the given consumeables by concatenating the last-emitted elements, separated by a space. Useful
    * for CSS. */
  def combineSpaced(fixed: String, first: Consumeable[String], others: Consumeable[String]*): Consumeable[String] = {
    def startWithEmpty = ZPipeline.prepend(Chunk(""))
    val streams = (first +: others.toSeq).map(_.via(startWithEmpty))
    combine(ZStream(fixed), streams: _*).map(_.mkString(" "))
  }

  /** Combines the given consumeables by concatenating the last-emitted elements. Only emits once all involved
    * consumeables have emitted their first element. To enforce a first element, use
    * .via(ZPipeline.prepend(value)). */
  def combine[T](first: Consumeable[T], others: Consumeable[T]*): Consumeable[Chunk[T]] = {
    val all = first.map(Chunk(_)) +: others.toSeq.map(_.map(Chunk(_)))
    all.reduce((a,b) => a.zipLatestWith(b)(_ ++ _))
  }

  val id = Attribute("id")
  val title = Attribute("title")
  val width = Attribute("width")
  val height = Attribute("height")
  // TODO: Have typesafe setters for CSS style directly? Perhaps?
  val style = Attribute("style")
  val name = Attribute("name")
  val checked = Attribute("checked")
  val tabindex = Attribute("tabindex")
  val placeholder = Attribute("placeholder")
  val href = Attribute("href")
  val list = Attribute("list")
  val value = Attribute("value")

  /** The CSS class(es) to set. Use Attribute.combine() if you have multiple sources. */
  val `class` = Attribute("class")
  /** Alias for `class` */
  val className = `class`
  /** Alias for `class` */
  val cls = `class`

  val `type` = Attribute("type")
  /** Alias for `type` */
  val typ = `type`

  val `for` = Attribute("for")

  // SVG attributes, keeping in shared scope for now
  val azimuth = Attribute("azimuth")
  val baseFrequency = Attribute("baseFrequency")
  val bias = Attribute("bias")
  val crossorigin = Attribute("crossorigin")
  val cx = Attribute("cx")
  val cy = Attribute("cy")
  val d = Attribute("d")
  val diffuseConstant = Attribute("diffuseConstant")
  val divisor = Attribute("divisor")
  val dx = Attribute("dx")
  val dy = Attribute("dy")
  val edgeMode = Attribute("edgeMode")
  val elevation = Attribute("elevation")
  val fill = Attribute("fill")
  val filter = Attribute("filter")
  val floodColor = Attribute("flood-color")
  val floodOpacity = Attribute("flood-opacity")
  val in = Attribute("in")
  val in2 = Attribute("in2")
  val k1 = Attribute("k1")
  val k2 = Attribute("k2")
  val k3 = Attribute("k3")
  val k4 = Attribute("k4")
  val kernelMatrix = Attribute("kernelMatrix")
  val kernelUnitLength = Attribute("kernelUnitLength")
  val limitingConeAngle = Attribute("limitingConeAngle")
  val markerHeight = Attribute("markerHeight")
  val markerWidth = Attribute("markerWidth")
  val mode = Attribute("mode")
  val numOctaves = Attribute("numOctaves")
  val operator = Attribute("operator")
  val order = Attribute("order")
  val orient = Attribute("orient")
  val overflow = Attribute("overflow")
  val pathLength = Attribute("pathLength")
  val pointsAtX = Attribute("pointsAtX")
  val pointsAtY = Attribute("pointsAtY")
  val pointsAtZ = Attribute("pointsAtZ")
  val preserveAlpha = Attribute("preserveAlpha")
  val preserveAspectRatio = Attribute("preserveAspectRatio")
  val r = Attribute("r")
  val radius = Attribute("radius")
  val refX = Attribute("refX")
  val refY = Attribute("refY")
  val result = Attribute("result")
  val scale = Attribute("scale")
  val seed = Attribute("seed")
  val specularConstant = Attribute("specularConstant")
  val specularExponent = Attribute("specularExponent")
  val stdDeviation = Attribute("stdDeviation")
  val stroke = Attribute("stroke")
  val surfaceScale = Attribute("surfaceScale")
  val switchTiles = Attribute("switchTiles")
  val tableValues = Attribute("tableValues")
  val targetX = Attribute("targetX")
  val targetY = Attribute("targetY")
  val textAnchor = Attribute("text-anchor")
  val transform = Attribute("transform")
  val values = Attribute("values")
  val viewBox = Attribute("viewBox")
  val visibility = Attribute("visibility")
  val x = Attribute("x")
  val xChannelSelector = Attribute("xChannelSelector")
  val y = Attribute("y")
  val yChannelSelector = Attribute("yChannelSelector")
  val z = Attribute("z")

}
