package zio.lazagna.dom

import zio.{Scope, ZIO}

import org.scalajs.dom

case class Element[E <: dom.Element](target: E, children: Seq[Modifier]) extends Modifier {
  override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
    mount(parent, None)
  }

  private[lazagna] def mount(parent: dom.Element, after: Option[dom.Element]): ZIO[Scope, Nothing, Unit] = {
    // We mount the children after the parent, so it's guaranteed to be in the DOM tree.
    val mountChildren = ZIO.collectAll(children.map(_.mount(target)))
    ZIO.acquireRelease {
      ZIO.succeed {
        after.map { a =>
          parent.insertBefore(target, a.nextSibling)
        }.getOrElse {
          parent.appendChild(target)
        }
      }
    } { _ =>
      ZIO.succeed {
        parent.removeChild(target)
      }
    }.unit <* mountChildren
  }

  private[lazagna] def moveAfter(parent: dom.Element, after: dom.Element): ZIO[Scope, Nothing, Unit] = {
    ZIO.succeed {
      parent.insertBefore(target, after.nextSibling)
    }
  }
}

object Element {
  val textContent = TextContent
  val children = Children

  def thisElementAs(fn: dom.Element => Modifier): Modifier = Modifier { parent =>
    fn.apply(parent).mount(parent)
  }

  /** Sets keyboard focus on the parent element directly after creating it (by calling element.focus()) */
  def focusNow: Modifier = Modifier {
    _ match {
      case e:dom.HTMLElement =>
        ZIO.succeed(e.focus())
      case _ =>
        ZIO.unit
    }
  }

  case class CreateFn(name: String) {
    def apply(children: Modifier*) = Element(dom.document.createElement(name), children.toSeq)
  }

  object tags {
    val div = CreateFn("div")
    val input = CreateFn("input")
    val label = CreateFn("label")
    val datalist = CreateFn("datalist")
    val option = CreateFn("option")
    /** The "style" tag, aliased to not conflict with the "style" attribute */
    val styleTag = CreateFn("style")
  }

  object svgtags {
    case class CreateFn[E <: dom.Element](name: String) {
      def apply(children: Modifier*) = Element[E](dom.document.createElementNS("http://www.w3.org/2000/svg", name).asInstanceOf[E], children.toSeq)
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
