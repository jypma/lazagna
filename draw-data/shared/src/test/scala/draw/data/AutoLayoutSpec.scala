package draw.data

import zio.test.ZIOSpecDefault
import zio.test._
import zio.test.Assertion._
import draw.data.drawevent.DrawingCreated
import draw.data.drawevent.IconCreated
import draw.data.drawevent.ObjectLabelled
import draw.data.drawevent.LinkCreated
import draw.data.drawevent.DrawEvent
import draw.data.drawevent.ObjectsLayedOut
import draw.data.drawevent.ObjectMoved
import draw.data.drawcommand.MoveObject
import draw.data.point.Point
import zio.Clock
import draw.data.drawcommand.DrawCommand
import draw.data.drawcommand.EditLink

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

    test("puts two icons from origin below each other (angle 180)") {
      val events = Seq(
        IconCreated("i1"),
        IconCreated("i2"),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1), preferredAngle = Some(180))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          isAt("i1", 0, 50) &&
            isAt("i2", 0, -50)
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

    test("puts two icons from origin next to each other (angle 270)") {
      val events = Seq(
        IconCreated("i1"),
        IconCreated("i2"),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1), preferredAngle = Some(270))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          isAt("i1", 50, 0) &&
            isAt("i2", -50, 0)
        )
      }
    },

    test("rotates an icon upwards from left") {
      val events = Seq(
        IconCreated("i1", position = Some(Point(0,0))),
        IconCreated("i2", position = Some(Point(-50,0))),
        IconCreated("i3", position = Some(Point(0,50))),
        LinkCreated("l1", "i1", "i2", preferredDistance = Some(1), preferredAngle = Some(0)),
        LinkCreated("l2", "i1", "i3", preferredDistance = Some(1), preferredAngle = Some(0))
      ).zipWithIndex.map { (body, idx) => DrawEvent(idx, body) }

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        res <- layout.performLayout(state)
      } yield {
        assert(res)(
          distanceBetween("i1", "i2", 100) &&
          angleBetween("i1", "i2", 0)
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

    test("rotates a link upwards from down") {
      val events = Seq(
        DrawEvent(1,DrawingCreated(),Some(1710920418522L)),
        DrawEvent(4797,IconCreated("AY7wBMpneXyl827/yeMaCA",Some(Point(326.28814697265625,194.70420837402344)),Some("elusive"),Some("elusive-person"),Some(25.477447509765625),Some(62.60443115234375)),Some(1713423829628L)),
        DrawEvent(4798,IconCreated("AY7wBM6NdsiP0dmos1D2ZA",Some(Point(543.5591430664062,194.1793975830078)),Some("elusive"),Some("elusive-person"),Some(25.47747802734375),Some(62.60443115234375)),Some(1713423830671L)),
        DrawEvent(4799,IconCreated("AY7wBNSHeFC1HpxujN1Zcw",Some(Point(762.4046020507812,188.93130493164062)),Some("elusive"),Some("elusive-person"),Some(25.47747802734375),Some(62.60443115234375)),Some(1713423832202L)),
        DrawEvent(4800,ObjectLabelled("AY7wBMpneXyl827/yeMaCA","a",Some(8.090807914733887),Some(23.87881636619568),Some(-3.148854970932007)),Some(1713423839585L)),
        DrawEvent(4801,ObjectLabelled("AY7wBM6NdsiP0dmos1D2ZA","b",Some(7.9508585929870605),Some(23.87881636619568),Some(-3.148854970932007)),Some(1713423841840L)),
        DrawEvent(4802,ObjectLabelled("AY7wBNSHeFC1HpxujN1Zcw","c",Some(7.18988561630249),Some(23.87881636619568),Some(-3.148854970932007)),Some(1713423843678L)),
        DrawEvent(4975,LinkCreated("AY7wBRX/dFWdab5w6RvutA","AY7wBMpneXyl827/yeMaCA","AY7wBM6NdsiP0dmos1D2ZA",Some(2),Some(180)),Some(1713426701115L)),
        DrawEvent(5034,LinkCreated("AY7wMGjXc6CL9/k7W/G2eQ","AY7wBMpneXyl827/yeMaCA","AY7wBNSHeFC1HpxujN1Zcw",Some(2),Some(360)),Some(1714376817333L)),
        DrawEvent(5035,ObjectsLayedOut(Vector(
          ObjectMoved("AY7wBNSHeFC1HpxujN1Zcw",Some(Point(565.9136652993325,327.2342160106803))),
          ObjectMoved("AY7wBMpneXyl827/yeMaCA",Some(Point(565.9237292040432,177.2242174157633))),
          ObjectMoved("AY7wBM6NdsiP0dmos1D2ZA",Some(Point(565.9137930661285,27.234218801235464)))
        )),Some(1713771015208L))
      )

      val state = events.foldLeft(DrawingState())((s, e) => s.update(e)._2)

      for {
        initial <- layout.performLayout(state)
        now <- Clock.instant
        state2 = state.handle(now, DrawCommand(EditLink("AY7wMGjXc6CL9/k7W/G2eQ", Some(2), Some(180)))).foldLeft(state)(_.update(_)._2)
        res <- layout.performLayout(state2)
      } yield {
        assert(initial)(isEmpty) && // Re-layout of the existing state should not move any objects.
        assert(res)(
          distanceBetween("AY7wBMpneXyl827/yeMaCA", "AY7wBM6NdsiP0dmos1D2ZA", 150) &&
          distanceBetween("AY7wBMpneXyl827/yeMaCA", "AY7wBNSHeFC1HpxujN1Zcw", 150) &&
          angleBetween("AY7wBMpneXyl827/yeMaCA", "AY7wBM6NdsiP0dmos1D2ZA", 180) &&
          angleBetween("AY7wBMpneXyl827/yeMaCA", "AY7wBNSHeFC1HpxujN1Zcw", 180)
        )
      }
    }

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

  inline def angleBetween(id1: String, id2: String, angle: Int): Assertion[Iterable[MoveObject]] = {
    assertion[Iterable[MoveObject]](s"Has angle ${angle} degrees between ${id1} and ${id2}") { res =>
      val d = for {
        m1 <- res.find(_.id == id1).flatMap(_.position)
        m2 <- res.find(_.id == id2).flatMap(_.position)
      } yield Math.atan2(m2.x - m1.x, m2.y - m1.y) * (180.0 / Math.PI)

      d.exists { v =>
        val found = (v.round + 360) % 360
        found == ((angle + 360) % 360)
      }
    }
  }

}
