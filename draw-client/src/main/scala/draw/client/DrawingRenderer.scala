package draw.client

import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.{children, textContent}
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.{PathData}
import zio.{ZIO, ZLayer}

import draw.data.drawevent.{ScribbleContinued, ScribbleDeleted, ScribbleStarted}
import draw.data.point.Point
import org.scalajs.dom
import draw.data.drawevent.DrawEvent
import zio.lazagna.dom.Alternative
import zio.stream.SubscriptionRef
import zio.lazagna.Consumeable._
import zio.stream.ZPipeline
import zio.Chunk

trait DrawingRenderer {
  def render: Modifier
}

object DrawingRenderer {
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
                val startData =
                  startPoints.headOption.map(start => PathData.MoveTo(start.x, start.y)) ++
                  startPoints.tail.map(pos => PathData.LineTo(pos.x, pos.y))
                val points = d <-- drawing.eventsAfter(sequenceNr)
                  .chunks
                  .takeUntil(_.exists(_ match { // FIXME Handle update and delete within same small time window
                    case DrawEvent(_, ScribbleDeleted(ID, _), _, _, _) => true
                    case _ => false
                  }))
                  .map(_
                    .collect { case DrawEvent(_, ScribbleContinued(ID, points, _), _, _, _) => points }
                    .flatMap(p => p)
                    .map { pos => PathData.LineTo(pos.x, pos.y) }
                  )
                  .via(ZPipeline.prepend(Chunk(Chunk.empty))) // in order to also trigger render on the initial starting points
                  .mapAccum(startData) { (seq, events) =>
                    val res = seq ++ events
                    (res, res)
                  }
                  .map(PathData.render)

                Some(children.Append(
                  g(
                    id := s"scribble${scribbleId}",
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
