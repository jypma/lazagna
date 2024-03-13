package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.{_}
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.PathData

import draw.data.{LinkState, ObjectState}
import draw.geom.{Point, Rectangle}

object LinkRenderer {
  def make = for {
    renderState <- ZIO.service[RenderState]
    iconRenderer <- ZIO.service[IconRenderer]
  } yield new ObjectRenderer[LinkState] {
    override def render(initial: ObjectState[LinkState], furtherEvents: Consumeable[LinkState]) = {
      val srcBoxes = iconRenderer.getBoundingBoxes(initial.body.src)
      val destBoxes = iconRenderer.getBoundingBoxes(initial.body.dest)

      val points = d <-- srcBoxes.zipLatestWith(destBoxes)((_, _)).map { case ((src, srcLabel), (dst, dstLabel)) =>
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
          points
        ),
        path(
          cls := "selectTarget editTarget",
          points
        ),
        thisElementAs { element => renderState.notifyRendered(RenderedObject(initial.id, initial.body, element)) }
      )
    }
  }
}
