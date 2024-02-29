package draw.client

import zio.lazagna.Consumeable._
import zio.lazagna.Consumeable.given
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.{textContent}
import zio.lazagna.dom.svg.PathData
import zio.lazagna.dom.{Alternative, Attribute, Modifier}
import zio.stream.{SubscriptionRef}
import zio.{ZIO, ZLayer}

import draw.data.point.Point
import org.scalajs.dom
import zio.lazagna.dom.Children
import Drawing._

trait DrawingRenderer {
  def render: Modifier
}

object DrawingRenderer {
  case class ObjectTarget(id: String, position: Point)
  object ObjectTarget {
    def apply(target: dom.Element): Option[ObjectTarget] = {
      val id = target.id match {
        case s if s.startsWith("scribble") => Some(s.substring("scribble".length))
        case s if s.startsWith("icon") => Some(s.substring("icon".length))
        case _ => None
      }
      id.map(i => ObjectTarget(i, Point(
        if (target.hasAttribute("data-x")) target.getAttribute("data-x").toDouble else 0,
        if (target.hasAttribute("data-y")) target.getAttribute("data-y").toDouble else 0)))
    }
  }

  val dataX = Attribute("data-x")
  val dataY = Attribute("data-y")

  /** Finds the element, or any of its parents, that has an ID, but only if one of those ancestors also has a
    * CSS "clickTarget" class (indicating that it's a valid selection target) */
  private def getClickTargetParent(e: dom.Element): Option[dom.Element] = {
    var elem = e
    var count = 3
    var isClickTarget = e.classList.contains("clickTarget")
    while (elem.id == "" && count > 0) {
      elem = elem.parentNode.asInstanceOf[dom.Element]
      if (elem.classList.contains("clickTarget")) {
        isClickTarget = true
      }
      count -= 1
    }
    Option.when(isClickTarget)(elem)
  }

  /** Returns information about an object that might have been clicked as an event target */
  def getTargetObject(event: dom.MouseEvent): Option[ObjectTarget] = {
    Some(event)
      .filter { e => (e.buttons & 1) != 0 }
      .map(_.target)
      .collect { case elem: dom.Element =>
        elem }
      .map { getClickTargetParent }
      .collect { case Some(e:dom.Element) => e }
      .flatMap(ObjectTarget.apply(_))
  }

  val live = ZLayer.fromZIO {
    for {
      drawingTools <- ZIO.service[DrawingTools]
      drawing <- ZIO.service[Drawing]
      initialView = if (drawing.initialVersion <= 1) 1 else 0
      currentView <- SubscriptionRef.make(initialView)
    } yield new DrawingRenderer {
      val start = System.currentTimeMillis()
      var eventCountDebug = 0

      val renderedObjects = Modifier.unwrap {
        for {
          children <- Children.make

          // TODO: Actually take the version of the last actually rendered state here
          _ <- drawing.currentVersion.filter(_ >= drawing.initialVersion).mapZIO { _ =>
            // Local: 0.5ms per event
            // LAN: 0.8ms per event
            // With IndexedDB (cursor), plus read from remote websocket: 3ms per event
            // With IndexedDB (cursor), plus read from local websocket: 2ms per event
            // With IndexedDB (cursor), from DB: 0.8ms per event
            val time = System.currentTimeMillis() - start
            println(s"Processed ${eventCountDebug} events, until sequence nr ${drawing.initialVersion}, in ${time}ms")
            currentView.set(1)
          }.take(1).consume

          _ <- drawing.initialObjectStates.mapZIO { initial =>
            eventCountDebug += 1
            val furtherEvents = drawing.objectState(initial.id).takeUntil(_.deleted)
            children.child { destroy =>
              g(
                Modifier.run(drawing.objectState(initial.id).filter(_.deleted).mapZIO(_ => destroy).take(1).consume),
                initial match {
                  case _:ScribbleState =>
                    val position = furtherEvents
                      .collect { case ScribbleState(_, _, pos, _) => pos }
                      .changes

                    val points = d <-- furtherEvents
                      .collect { case ScribbleState(_, _, _, p) => p }
                      .map { p =>
                        p.headOption.map(start => PathData.MoveTo(start.x, start.y)) ++
                        p.tail.map(pos => PathData.LineTo(pos.x, pos.y))
                      }
                      .map(PathData.render)
                      .changes

                    g(
                      cls := "scribble",
                      id := s"scribble${initial.id}",
                      transform <-- position.map(p => s"translate(${p.x},${p.y})"),
                      dataX <-- position.map(_.x),
                      dataY <-- position.map(_.y),
                      path(
                        points
                      ),
                      path(
                        cls := "clickTarget",
                        points
                      )
                    )

                  case IconState(_,_,_,symbol,_) =>
                    val position = furtherEvents
                      .collect { case IconState(_, _, pos, _, _) => pos }
                      .changes

                    g(
                      id := s"icon${initial.id}",
                      cls := "icon",
                      transform <-- position.map(p => s"translate(${p.x},${p.y})"),
                      dataX <-- position.map(_.x),
                      dataY <-- position.map(_.y),
                      g(
                        cls := "clickTarget",
                        use(
                          svgTitle(textContent := symbol.name),
                          href := symbol.href,
                          cls := "icon",
                          width := 64, // TODO: share iconSize between preview in DrawingTools and actual rendering here. Probably share the rendering code itself.
                          height := 64,
                          x := -32,
                          y := -32
                        ),
                      ),
                      text(
                        cls := "label",
                        x := 0,
                        y := 32,
                        textContent <-- furtherEvents
                          .collect { case IconState(_,_,_,_,label) => label }
                      )
                    )
                }
              )
            }
          }.consume
        } yield Modifier.combine (
          children.render
        )
      }

      val render = {
        val svgLoading = div(
          cls := "loading",
          textContent := "Loading..."
        )

        val svgDisconnected = div(
          cls := "disconnected",
          textContent := "The server has disconnected. Please reload this page to reconnect."
        )

        val svgMain = div(
          svg(
            svgStyleTag(), // No direct style here, stuff is added here when exporting.
            cls <-- drawingTools.currentToolName.map(t => s"main tool-${t}"),
            viewBox <-- drawing.viewport.map(_.toSvgViewBox),
            overflow := "hidden",
            tabindex := 0, // To enable keyboard events
            g(
              renderedObjects
            ),
            drawingTools.renderHandlers
          )
        )

        val alternatives = Map(
          0 -> svgLoading,
          1 -> svgMain,
          2 -> svgDisconnected
        )
        Alternative.showOne(currentView.merge(drawing.connectionStatus.collect {
          case Drawing.Disconnected => 2
        }), alternatives, Some(initialView))
      }
    }
  }
}
