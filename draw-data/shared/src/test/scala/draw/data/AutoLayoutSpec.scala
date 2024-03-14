package draw.data

import zio.test.ZIOSpecDefault
import zio.test._
import zio.test.Assertion._
import draw.data.drawevent.IconCreated
import draw.data.drawevent.LinkCreated
import draw.data.drawevent.DrawEvent
import draw.data.drawcommand.MoveObject
import draw.data.point.Point

object AutoLayoutSpec extends ZIOSpecDefault {
  val layout = new AutoLayoutImpl()

  override def spec = suite("AutoLayoutSpec")(
    test("puts two icons from initial position above each other (angle 0)") {
      val events = Seq(
        IconCreated("i1", position = Some(Point(1,1))),
        IconCreated("i2", position = Some(Point(2,2))),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1), preferredAngle = Some(0))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          isAt("i1", 1.5, -48.5) &&
            isAt("i2", 1.5, 51.5)
        )
      }
    },

    test("puts two icons from origin above each other (angle 0)") {
      val events = Seq(
        IconCreated("i1"),
        IconCreated("i2"),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1), preferredAngle = Some(0))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          isAt("i1", 0, -50) &&
            isAt("i2", 0, 50)
        )
      }
    },

    test("puts two icons from origin next to each other (angle 90)") {
      val events = Seq(
        IconCreated("i1"),
        IconCreated("i2"),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1), preferredAngle = Some(90))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          isAt("i1", -50, 0) &&
            isAt("i2", 50, 0)
        )
      }
    },

    test("puts three icons from origin into a triangle") {
      val events = Seq(
        IconCreated("i1"),
        IconCreated("i2"),
        IconCreated("i3"),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1)),
        LinkCreated("l2", "i2", "i3", preferredDistance = Some(1)),
        LinkCreated("l3", "i3", "i1", preferredDistance = Some(1))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          distanceBetween("i1", "i2", 100) &&
          distanceBetween("i2", "i3", 100) &&
          distanceBetween("i3", "i1", 100)
        )
      }
    },

  )

  inline def isAt(id: String, x: Double, y: Double): Assertion[Iterable[MoveObject]] = {
    exists(
      hasField("id", (m:MoveObject) => m.id, equalTo(id)) &&
      hasField("position.x", (m:MoveObject) => m.position.map(_.x).getOrElse(0.0), approximatelyEquals(x, 0.1))  &&
      hasField("position.y", (m:MoveObject) => m.position.map(_.y).getOrElse(0.0), approximatelyEquals(y, 0.1))
    )
  }

  inline def distanceBetween(id1: String, id2: String, distance: Double): Assertion[Iterable[MoveObject]] = {
    assertion[Iterable[MoveObject]](s"Has distance ${distance} between ${id1} and ${id2}") { res =>
      val d = for {
        m1 <- res.find(_.id == id1).flatMap(_.position)
        m2 <- res.find(_.id == id2).flatMap(_.position)
      } yield Math.pow(Math.pow(m1.x - m2.x, 2) + Math.pow(m1.y - m2.y, 2), 0.5)

      d.exists { v =>
        Math.abs(v - distance) < 0.1
      }
    }
  }
}
