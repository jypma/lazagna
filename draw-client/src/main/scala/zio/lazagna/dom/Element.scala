package zio.lazagna.dom

import zio.{Scope, ZIO}

import org.scalajs.dom
import Modifier.MountPoint
import zio.ZLayer

object Element {
  val textContent = TextContent
  val children = Children

  // TODO: Consider making MountPoint generic, so we can get a concrete type here
  def thisElementAs[T](fn: dom.Element => Modifier[T]): Modifier[T] = Modifier { parent =>
    fn.apply(parent)
  }

  /** Sets keyboard focus on the parent element directly after creating it (by calling element.focus()) */
  def focusNow: Modifier[Unit] = Modifier {
    _ match {
      case e:dom.HTMLElement =>
        ZIO.succeed(e.focus())
      case _ =>
        ZIO.unit
    }
  }

  private def mkElement[E <: dom.Element](target: E, children: Seq[Modifier[_]]): Modifier[E] = ZIO.service[MountPoint].flatMap { i =>
    // We mount the children after the parent, so it's guaranteed to be in the DOM tree.
    val mountChildren = ZIO.collectAll(children).provideSome[Scope](ZLayer.succeed(MountPoint(target)))
    ZIO.acquireRelease {
      ZIO.succeed {
        i.after.map { a =>
          i.parent.insertBefore(target, a.nextSibling)
        }.getOrElse {
          i.parent.appendChild(target)
        }
        target
      }
    } { _ =>
      ZIO.succeed {
        i.parent.removeChild(target)
      }
    } <* mountChildren
  }

  case class CreateFn[E <: dom.Element](name: String) {
    def apply(children: Modifier[_]*): Modifier[E] = mkElement(dom.document.createElement(name).asInstanceOf[E], children.toSeq)
  }

  object tags {
    val div = CreateFn[dom.HTMLElement]("div")
    val input = CreateFn[dom.HTMLInputElement]("input")
    val textarea = CreateFn[dom.HTMLTextAreaElement]("input")
    val label = CreateFn[dom.HTMLElement]("label")
    val datalist = CreateFn[dom.HTMLElement]("datalist")
    val option = CreateFn[dom.HTMLElement]("option")
    /** The "style" tag, aliased to not conflict with the "style" attribute */
    val styleTag = CreateFn[dom.HTMLElement]("style")
  }

  object svgtags {
    case class CreateFn[E <: dom.Element](name: String) {
      def apply(children: Modifier[_]*) = mkElement[E](dom.document.createElementNS("http://www.w3.org/2000/svg", name).asInstanceOf[E], children.toSeq)
    }

    val svg = CreateFn[dom.svg.SVG]("svg")
    val rect = CreateFn[dom.svg.RectElement]("rect")
    val circle = CreateFn[dom.svg.Circle]("circle")
    val g = CreateFn[dom.svg.G]("g")
    val path = CreateFn[dom.svg.Path]("path")
    val text = CreateFn[dom.svg.Text]("text")
    val image = CreateFn[dom.svg.Image]("image")
    val use = CreateFn[dom.svg.Use]("use")
    /** The "title" tag, aliased to not conflict with the "title" attribute */
    val svgTitle = CreateFn[dom.svg.Use]("title")
    /** The "style" tag, aliased to not conflict with the "style" attribute */
    val svgStyleTag = CreateFn("style")
  }
}
