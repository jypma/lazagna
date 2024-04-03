package draw.client.render

import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.{textContent, _}
import zio.lazagna.dom.svg.SVGHelper
import zio.lazagna.dom.{Alternative, Attribute, Children, Element, Modifier}
import zio.stream.SubscriptionRef
import zio.{ZIO, ZLayer}

import draw.client.Drawing
import draw.client.tools.DrawingTools
import draw.data.{IconState, LinkState, ScribbleState}
import draw.geom.Point
import org.scalajs.dom

import Drawing._

trait DrawingRenderer {
  def render: Modifier[Any]
}

object DrawingRenderer {
  case class ObjectTarget(id: String, position: Point)
  object ObjectTarget {
    def apply(target: dom.Element): Option[ObjectTarget] = {
      val id = target.id match {
        case s if s.startsWith("scribble") => Some(s.substring("scribble".length))
        case s if s.startsWith("icon") => Some(s.substring("icon".length))
        case s if s.startsWith("link") => Some(s.substring("link".length))
        case _ => None
      }
      id.map(i => ObjectTarget(i, Point(
        if (target.hasAttribute("data-x")) target.getAttribute("data-x").toDouble else 0,
        if (target.hasAttribute("data-y")) target.getAttribute("data-y").toDouble else 0)))
    }
  }

  val iconSize = 64

  val live = ZLayer.fromZIO {
    for {
      drawingTools <- ZIO.service[DrawingTools]
      drawing <- ZIO.service[Drawing]
      renderState <- ZIO.service[RenderState]
      initialView = if (drawing.initialVersion <= 1) 1 else 0
      currentView <- SubscriptionRef.make(initialView)
    } yield new DrawingRenderer {
      println("Rendering up to event " + drawing.initialVersion + " in background.")
      val start = System.currentTimeMillis()

      def render = {
        val svgMain = div(
          svg(
            svgStyleTag(), // No direct style here, stuff is added here when exporting.
            cls <-- drawingTools.currentToolName.map(t => s"main tool-${t}"),
            viewBox <-- drawing.viewport.map(_.toSvgViewBox()),
            overflow := "hidden",
            tabindex := 0, // To enable keyboard events
            focusNow, // To allow dragging immediately
            SVGHelper { helper =>
              val deps = ZLayer.succeed(renderState) ++ ZLayer.succeed(helper) ++ ZLayer.succeed(drawing)
              g(
                cls := "drawing",
                for {
                  scribbleR <- ScribbleRenderer.make.provide(deps)
                  iconR <- IconRenderer.make.provide(deps)
                  linkR <- LinkRenderer.make.provide(deps, ZLayer.succeed(iconR))
                  children <- Children.make
                  _ <- drawing.initialObjectStates.mapZIO { initial =>
                    val renderer = (initial.body match {
                      case _:ScribbleState => scribbleR
                      case _:IconState => iconR
                      case _:LinkState => linkR
                    }).asInstanceOf[ObjectRenderer[initial.Body]]

                    children.child { destroy =>
                      g(
                        cls <-- renderState.selectionIds.map { s => if (s.contains(initial.id)) "selected" else "" }.changes,
                        renderer.render(initial).flatMap { case (elem, pipeline)  =>
                          drawing.objectState(initial)
                            .via(pipeline)
                            .tap(s => renderState.notifyRendered(s, elem) *> ZIO.when(s.deleted)(destroy))
                            .takeUntil(_.deleted)
                            .consume
                        }
                      )
                    }
                  }.consume
                  res <- children.render
                } yield res
              )
            },
            drawingTools.renderHandlers
          )
        )

        val alternatives = Map(
          0 -> div(
            cls := "loading",
            textContent := "Loading..."
          ),
          1 -> svgMain,
          2 -> div(
            cls := "disconnected",
            textContent := "The server has disconnected. Please reload this page to reconnect."
          )
        )

        var eventCountDebug = 0
        var switchedReady = false
        Modifier.all(
          Alternative.showOne(currentView.merge(drawing.connectionStatus.collect {
            case Drawing.Disconnected => 2
          }), alternatives, Some(initialView)),
          renderState.latestSequenceNr.tap { seqNr =>
            eventCountDebug += 1
            if (switchedReady || (seqNr < drawing.initialVersion)) ZIO.unit else {
              switchedReady = true
              val time = System.currentTimeMillis() - start
              println(s"Processed ${eventCountDebug} events, until sequence nr ${drawing.initialVersion}, in ${time}ms")
              currentView.set(1)
            }
          }.takeUntil(_ >= drawing.initialVersion).consume
        )
      }
    }
  }
}
