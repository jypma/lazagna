package draw.client

import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.{children, textContent}
import zio.lazagna.dom.svg.PathData
import zio.lazagna.dom.{Alternative, Modifier}
import zio.stream.{SubscriptionRef, ZPipeline}
import zio.{Chunk, ZIO, ZLayer}

import draw.data.drawevent.{DrawEvent, ScribbleContinued, ScribbleDeleted, ScribbleStarted, ObjectMoved}
import draw.data.point.Point
import org.scalajs.dom

trait DrawingRenderer {
  def render: Modifier
}

object DrawingRenderer {
  case class ObjectTarget(id: String, position: Point)

  val dataX = Attribute("data-x")
  val dataY = Attribute("data-y")

  /** Returns information about an object that might have been clicked as an event target */
  def getTargetObject(event: dom.MouseEvent): Option[ObjectTarget] = {
    Some(event)
      .filter { e => (e.buttons & 1) != 0 }
      .map(_.target)
      .collect { case elem: dom.Element =>
        elem }
      .map(_.parentNode)
      .collect {
        case parent:dom.Element if parent.id.startsWith("scribble") =>
          ObjectTarget(parent.id.substring("scribble".length), Point(
            if (parent.hasAttribute("data-x")) parent.getAttribute("data-x").toDouble else 0,
            if (parent.hasAttribute("data-y")) parent.getAttribute("data-y").toDouble else 0))
      }
  }

  val live = ZLayer.fromZIO {
    for {
      drawingTools <- ZIO.service[DrawingTools]
      drawing <- ZIO.service[Drawing]
      initialView = if (drawing.initialVersion <= 1) 1 else 0
      currentView <- SubscriptionRef.make(initialView)
    } yield new DrawingRenderer {
      val start = System.currentTimeMillis()

      override val render = {
        var eventCountDebug = 0
        val svgBody = g(
          children <~~ drawing.events
            .tap { event =>
              eventCountDebug += 1
              ZIO.when(event.sequenceNr == drawing.initialVersion)(currentView.set(1).tap(_ => ZIO.succeed{
                // Local: 0.5ms per event
                // LAN: 0.8ms per event
                // With IndexedDB (cursor), plus read from remote websocket: 3ms per event
                // With IndexedDB (cursor), plus read from local websocket: 2ms per event
                // With IndexedDB (cursor), from DB: 0.8ms per event
                val time = System.currentTimeMillis() - start
                println(s"Loaded ${eventCountDebug} events, until sequence nr ${drawing.initialVersion}, in ${time}ms")
              }))
            }
            .map {
              case DrawEvent(sequenceNr, ScribbleStarted(scribbleId, startPoints, _), _, _, _) =>
                val ID = scribbleId
                val furtherEvents = drawing.eventsAfter(sequenceNr)
                  .takeUntil(_ match {
                    case DrawEvent(_, ScribbleDeleted(ID, _), _, _, _) => true
                    case _ => false
                  })
                val startData =
                  startPoints.headOption.map(start => PathData.MoveTo(start.x, start.y)) ++
                  startPoints.tail.map(pos => PathData.LineTo(pos.x, pos.y))
                val points = d <-- furtherEvents
                  .collect { case DrawEvent(_, ScribbleContinued(ID, points, _), _, _, _) => points }
                  .map(_.map { pos => PathData.LineTo(pos.x, pos.y) })
                  .via(ZPipeline.prepend(Chunk(Chunk.empty))) // in order to also trigger render on the initial starting points
                  .mapAccum(startData) { (seq, events) =>
                    val res = seq ++ events
                    (res, res)
                  }
                  .map(PathData.render)

                val position = furtherEvents
                  .collect { case DrawEvent(_, ObjectMoved(ID, Some(position), _), _, _, _) => position}

                Some(children.Append(
                  g(
                    cls := "scribble",
                    id := s"scribble${scribbleId}",
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
                ))

              case DrawEvent(_, ScribbleDeleted(id, _), _, _, _) =>
                dom.document.getElementById(s"scribble${id}") match {
                  case null => None
                  case domElmt => Some(children.DeleteDOM(domElmt))
                }

              case _ =>
                None
            }
            .collect { case Some(op) => op }
        )

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
            cls <-- drawingTools.currentToolName.map(t => s"main tool-${t}"),
            viewBox <-- drawing.viewport.map(_.toSvgViewBox),
            overflow := "hidden",
            tabindex := 0, // To enable keyboard events
            svgBody,
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
