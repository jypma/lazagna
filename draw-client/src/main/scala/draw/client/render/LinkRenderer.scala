package draw.client.render

import zio.ZIO
import zio.lazagna.Consumeable
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.{_}
import zio.lazagna.dom.Element.svgtags._
import zio.lazagna.dom.Modifier
import zio.lazagna.dom.svg.{PathData, SVGHelper}

import draw.client.Drawing
import draw.data.{IconState, LinkState, ObjectState}
import draw.geom.{Point, Rectangle}
import org.scalajs.dom

object LinkRenderer {
  def make = for {
    drawing <- ZIO.service[Drawing]
    helper <- ZIO.service[SVGHelper]
  } yield new ObjectRenderer[LinkState] {
    def getBoundingBox(in: Consumeable[ObjectState[_]]) = in.collect {
      case ObjectState(id, _, _, _:IconState) =>
        val icon = dom.document.getElementById(s"icon${id}")
        val main = helper.svgBoundingBox(icon.querySelector(".selectTarget").asInstanceOf[dom.SVGLocatable], 5)
        val label = Option(icon.querySelector(".label"))
          .filter(!_.innerHTML.isEmpty)
          .map(e => helper.svgBoundingBox(e.asInstanceOf[dom.SVGLocatable], 0))
        (main, label)
    }.changes

    override def render(initial: ObjectState[LinkState], furtherEvents: Consumeable[LinkState]): Modifier = {
      val srcBoxes = getBoundingBox(drawing.objectState(initial.body.src))
      val destBoxes = getBoundingBox(drawing.objectState(initial.body.dest))

      val points = d <-- srcBoxes.zipLatestWith(destBoxes)((_, _)).map { case ((src, srcLabel), (dst, dstLabel)) =>
        // FIXME: sometimes the pointOnRect doesn't cut the line properly on the initial render
        // This is, because in those cases we're rendering this link BEFORE we've actually put the icons on the screen.
        // Solution A: Have a SubscriptionRef per object bounding box, which we calculate whenever the underlying element changes. Somehow sync on creation.
        // Solution B: Have a Hub per object which we send "ready" whenever we've done rendering an update
        // Solution C: Keep a SubscriptionRef with the actual SVG element per object, AND a hub with updates (receiving the ObjectState that triggered the update)
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
        )
      )

    }
  }
}
