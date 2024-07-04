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
  val stroke = Attribute("stroke")
  val fill = Attribute("fill")
  val cx = Attribute("cx")
  val cy = Attribute("cy")
  val r = Attribute("r")
  val d = Attribute("d")
  val pathLength = Attribute("pathLength")
  val x = Attribute("x")
  val y = Attribute("y")
  val refX = Attribute("refX")
  val refY = Attribute("refY")
  val markerWidth = Attribute("markerWidth")
  val markerHeight = Attribute("markerHeight")
  val orient = Attribute("orient")
  val textAnchor = Attribute("text-anchor")
  val viewBox = Attribute("viewBox")
  val overflow = Attribute("overflow")
  val preserveAspectRatio = Attribute("preserveAspectRatio")
  val transform = Attribute("transform")
  val visibility = Attribute("visibility")
}
