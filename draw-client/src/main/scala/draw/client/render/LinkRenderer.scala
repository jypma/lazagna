package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.{_}
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.MultiUpdate
import zio.lazagna.dom.svg.PathData

import draw.data.{LinkState, ObjectState}
import draw.geom.{Point, Rectangle}
import zio.stream.ZPipeline
import zio.Promise
import draw.client.Drawing

object LinkRenderer {
  private val margin = 4

  def make = for {
    renderState <- ZIO.service[RenderState]
    iconRenderer <- ZIO.service[IconRenderer]
    ready <- Promise.make[Nothing,Unit]
    drawing <- ZIO.service[Drawing]
  } yield new ObjectRenderer[LinkState] {
    def getBoundingBoxes(iconId: String): Consumeable[(Rectangle, Option[Rectangle])] =
      drawing.objectState(iconId).map(_.body).collect { state =>
        val boxes = state.boundingBoxes
        (boxes.head.expand(margin), boxes.lastOption.map(_.expand(margin)))
      }

    override def render (initial: ObjectState[LinkState]) = MultiUpdate.make[String].flatMap { pointsUpdate =>
      val srcBoxes = getBoundingBoxes(initial.body.src)
      val destBoxes = getBoundingBoxes(initial.body.dest)

      val points = srcBoxes.zipLatestWith(destBoxes)((_, _)).map { case ((src, srcLabel), (dst, dstLabel)) =>
        val fullLine = src.middle.to(dst.middle)

        def intersect(icon: Rectangle, label: Option[Rectangle]) = label.filter(_.intersects(fullLine)).map { r =>
          Rectangle(Point(r.origin.x, icon.origin.y), r.width, r.origin.y - icon.origin.y + r.height)
        }.getOrElse {
          icon
        }

        val srcBox = intersect(src, srcLabel)
        val dstBox = intersect(dst, dstLabel)
        val dp = src.middle.toIntersection(dstBox).to
        val sp = dst.middle.toIntersection(srcBox).to
        PathData.render(Seq(
          PathData.MoveTo(sp.x, sp.y),
          PathData.LineTo(dp.x, dp.y)
        ))
      }

      g(
        id := s"link${initial.id}",
        cls := "link",
        path(
          pointsUpdate { s =>
            d := s
          }
        ),
        path(
          cls := "selectTarget editTarget",
          pointsUpdate { s =>
            d := s
          }
        ),
        thisElementAs { element =>
          points.via(pointsUpdate.pipeline).tap { _ =>
            renderState.notifyRendered(initial, element) *> ready.completeWith(ZIO.unit)
          }.consume
        }
      ).map((_, ZPipeline.tap(_ => ready.await))) // No updates rendered on Link itself
    }
  }
}
