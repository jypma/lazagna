package zio.lazagna.dom

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.{Scope, ZIO}

import org.scalajs.dom

case class Attribute(name: String) {
  def :=(value: Double): Modifier = :=(value.toString)

  def :=(value: Int): Modifier = :=(value.toString)

  def :=(value: String): Modifier = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      ZIO.succeed {
        parent.setAttribute(name, value)
      }
    }
  }

  def <--(content: Consumeable[String]) = new Modifier {
    override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
      content.map { value =>
        parent.setAttribute(name, value)
      }.consume
    }
  }
}

object Attribute {
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

  // TODO: Allow combining of multiple Consumeable[_,String] to set className from several sources, using ZStream.zipLatestWith
  // Syntax would be a function Attribute.combine(Consumeable[_,String]*)
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
  val fill = Attribute("fill")
  val cx = Attribute("cx")
  val cy = Attribute("cy")
  val r = Attribute("r")
  val d = Attribute("d")
  val pathLength = Attribute("pathLength")
  val x = Attribute("x")
  val y = Attribute("y")
  val textAnchor = Attribute("text-anchor")
  val viewBox = Attribute("viewBox")
  val overflow = Attribute("overflow")
  val preserveAspectRatio = Attribute("preserveAspectRatio")
}
