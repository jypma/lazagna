package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable._
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.{_}
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.svg.PathData

import draw.data.{LinkState, ObjectState}
import draw.geom.{Point, Rectangle}
import zio.lazagna.dom.MultiUpdate

object LinkRenderer {
  def make = for {
    renderState <- ZIO.service[RenderState]
    iconRenderer <- ZIO.service[IconRenderer]
  } yield new ObjectRenderer[LinkState] {
    override def render(initial: ObjectState[LinkState], furtherEvents: Consumeable[LinkState]) = for {
      pointsUpdate <- MultiUpdate.make[String]
      res <- {
        val srcBoxes = iconRenderer.getBoundingBoxes(initial.body.src)
        val destBoxes = iconRenderer.getBoundingBoxes(initial.body.dest)

        val points = srcBoxes.zipLatestWith(destBoxes)((_, _)).map { case ((src, srcLabel), (dst, dstLabel)) =>
          val fullLine = src.middle.to(dst.middle)

          def intersect(icon: Rectangle, label: Option[Rectangle]) = label.filter(_.intersects(fullLine)).map { r =>
            Rectangle(Point(r.origin.x, icon.origin.y), r.width, r.origin.y - icon.origin.y + r.height)
          }.getOrElse(icon)

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
            points.via(pointsUpdate.pipeline).tap(_ => renderState.notifyRendered(initial.id, initial.body, element)).consume
          }
        )
      }
    } yield res
  }
}
