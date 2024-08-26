package zio.lazagna.dom

import zio.{Scope, ZIO, ZLayer}
import zio.lazagna.Setup

import org.scalajs.dom

import Modifier.MountPoint

object Element {
  val textContent = TextContent
  val children = Children

  // TODO: Consider making MountPoint generic, so we can get a concrete type here
  def thisElementAs[R,E,T](fn: dom.Element => ZIO[R,E,T]): ZIO[R & MountPoint,E,T] =
    ZIO.service[MountPoint].flatMap { p => fn(p.parent) }

  /** Sets keyboard focus on the parent element directly after creating it (by calling element.focus()) */
  def focusNow: Modifier[Unit] = Modifier {
    _ match {
      case e:dom.HTMLElement =>
        ZIO.serviceWithZIO[Setup](_.addStartAction(ZIO.succeed(e.focus())))
      case _ =>
        ZIO.unit
    }
  }

  private def mkElement[E <: dom.Element](target: E, children: Seq[Modifier[_]]): Modifier[E] = ZIO.service[MountPoint].flatMap { i =>
    // We mount the children before the parent, so we minimize the amount of render steps.
    // If an operation requires the parent to be in the DOM, it should use Setup.
    val mountChildren = ZIO.collectAll(children).provideSome[Scope & Setup]{ZLayer.succeed(MountPoint(target))}
    mountChildren *> ZIO.acquireRelease {
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
    }
  }

  case class CreateFn[E <: dom.Element](name: String) {
    def apply(children: Modifier[_]*): Modifier[E] = mkElement(dom.document.createElement(name).asInstanceOf[E], children.toSeq)
  }

  object tags {
    val div = CreateFn[dom.HTMLElement]("div")
    val span = CreateFn[dom.HTMLElement]("span")
    val a = CreateFn[dom.HTMLElement]("a")
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
    val defs = CreateFn[dom.svg.Use]("defs")
    val marker = CreateFn[dom.svg.Use]("marker")
    /** The "title" tag, aliased to not conflict with the "title" attribute */
    val svgTitle = CreateFn[dom.svg.Use]("title")
    /** The "style" tag, aliased to not conflict with the "style" attribute */
    val svgStyleTag = CreateFn("style")
    /** The "filter" tag, aliased to not conflict with the "filter" attribute */
    val filterTag = CreateFn[dom.svg.Filter]("filter")
    val feBlend = CreateFn[dom.svg.FEBlend]("feBlend")
    val feColorMatrix = CreateFn[dom.svg.FEColorMatrix]("feColorMatrix")
    val feComponentTransfer = CreateFn[dom.svg.FEComponentTransfer]("feComponentTransfer")
    val feComposite = CreateFn[dom.svg.FEComposite]("feComposite")
    val feDiffuseLighting = CreateFn[dom.svg.FEDiffuseLighting]("feDiffuseLighting")
    val feDisplacementMap = CreateFn[dom.svg.FEDisplacementMap]("feDisplacementMap")
    val feDistantLight = CreateFn[dom.svg.FEDistantLight]("feDistantLight")
    val feDropShadow = CreateFn[dom.SVGElement]("feDropShadow")
    val feFlood = CreateFn[dom.svg.FEFlood]("feFlood")
    val feFuncA = CreateFn[dom.svg.FEFuncA]("feFuncA")
    val feFuncB = CreateFn[dom.svg.FEFuncB]("feFuncB")
    val feFuncG = CreateFn[dom.svg.FEFuncG]("feFuncG")
    val feFuncR = CreateFn[dom.svg.FEFuncR]("feFuncR")
    val feGaussianBlur = CreateFn[dom.svg.FEFuncR]("feGaussianBlur")
    val feImage = CreateFn[dom.svg.FEImage]("feImage")
    val feMerge = CreateFn[dom.svg.FEMerge]("feMerge")
    val feMergeNode = CreateFn[dom.svg.FEMergeNode]("feMergeNode")
    val feMorphology = CreateFn[dom.svg.FEMorphology]("feMorphology")
    val feOffset = CreateFn[dom.svg.FEOffset]("feOffset")
    val fePointLight = CreateFn[dom.svg.FEPointLight]("fePointLight")
    val feSpecularLighting = CreateFn[dom.svg.FESpecularLighting]("feSpecularLighting")
    val feSpotLight = CreateFn[dom.svg.FESpotLight]("feSpotLight")
    val feTile = CreateFn[dom.svg.FETile]("feTile")
    val feTurbulence = CreateFn[dom.svg.FETurbulence]("feTurbulence")
  }
}
