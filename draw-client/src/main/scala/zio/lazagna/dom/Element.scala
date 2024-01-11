package zio.lazagna.dom

import org.scalajs.dom
import zio.ZIO
import zio.Scope

case class Element(target: dom.Element, children: Seq[Modifier]) extends Modifier {
  override def mount(parent: dom.Element): ZIO[Scope, Nothing, Unit] = {
    mount(parent, None)
  }

  private[lazagna] def mount(parent: dom.Element, after: Option[dom.Element]): ZIO[Scope, Nothing, Unit] = {
    dom.console.log("Mounting")
    val ch = ZIO.collectAll(children.map(_.mount(target)))
    ZIO.acquireRelease {
      ZIO.succeed {
        dom.console.log("Appending to parent:")
        dom.console.log(target)
        dom.console.log(parent)
        after.map { a =>
          parent.insertBefore(target, a.nextSibling)
        }.getOrElse {
          parent.appendChild(target)
        }
        dom.console.log("now has " + parent.childElementCount)
      }
    } { _ =>
      ZIO.succeed {
        dom.console.log("Removing from parent")
        parent.removeChild(target)
      }
    }.unit <* ch
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

  def div(children: Modifier*) = {
    val res = dom.document.createElement("div")
    Element(res, children.toSeq)
  }
}
