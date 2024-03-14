package draw.data

import zio.{UIO}
import draw.micrograd.Value
import draw.data.drawcommand.MoveObject
import draw.data.point.Point
import zio.Random
import zio.ZIO

trait AutoLayout {
  def performLayout(state: DrawingState): UIO[Seq[MoveObject]]
}

case class AutoLayoutImpl() {
  import AutoLayout._

  def performLayout(state: DrawingState): UIO[Seq[MoveObject]] = {
    val stateLinks = state.links
    if (stateLinks.isEmpty) ZIO.succeed(Seq.empty) else for {
      targets <- ZIO.collectAll(
        stateLinks
          .flatMap(l => state.objects.get(l.body.src).toSeq ++ state.objects.get(l.body.dest).toSeq)
          .map(s => Target.make(s.asInstanceOf[ObjectState[Moveable]]).map((s.id, _)))
      ).map(_.toMap)
    } yield {
      val params = stateLinks.map { link =>
        (link, targets.get(link.body.src), targets.get(link.body.dest))
      }.collect {
        case (link, Some(src), Some(dest)) if link.body.preferredAngle.isDefined || link.body.preferredDistance.isDefined =>
          (link, src, dest)
      }.map(Link(_,_,_))

      def performStep(stepSize: Double) = {
        val loss = params.foldLeft(Value.zero)(_ + _.loss)
        loss.backward()
        //println("loss: " + loss)
        //targets.values.foreach(t => println(s"id=${t.obj.id} x=${t.x} y=${t.y}"))
        params.foreach(_.adjust(stepSize))
      }

      for (i <- 1 to maxSteps) {
        performStep(stepSize)
      }

      targets.values.flatMap(_.toCommand).toSeq
    }
  }
}

object AutoLayout {
  val epsilon = 0.01
  val stepSize = 0.1
  val angleWeight = 0.03
  val maxSteps = 100

  case class Target(obj: ObjectState[Moveable], dx: Double, dy: Double) {
    val x:Value = obj.body.position.x + dx
    val y:Value = obj.body.position.y + dy

    def adjust(stepSize: Double): Unit = {
      x.adjust(stepSize)
      y.adjust(stepSize)
    }

    def toCommand: Option[MoveObject] = {
      if (Math.abs(x.value - obj.body.position.x) > 1 || Math.abs(y.value - obj.body.position.y) > 1) {
        Some(MoveObject(obj.id, Some(Point(x.value, y.value))))
      } else None
    }
  }
  object Target {
    def make(obj: ObjectState[Moveable]) = for {
      dx <- Random.nextDoubleBetween(-epsilon, epsilon)
      dy <- Random.nextDoubleBetween(-epsilon, epsilon)
    } yield Target(obj, dx, dy)
  }

  case class Link(link: ObjectState[LinkState], src:Target, dst:Target) {
    def loss: Value = {
      val distance = ((src.x - dst.x + epsilon).pow(2) + (src.y - dst.y + epsilon).pow(2)).pow(0.5)
      val distance_loss = getPreferredDistance(link).map { preferred =>
        (distance - preferred).pow(2)
      }.getOrElse(Value.zero)
      //println(s"${link.id} has distance ${distance} with loss ${distance_loss}")

      val angle = (dst.y - src.y + epsilon).atan2(dst.x - src.x + epsilon) * (180.0 / Math.PI)
      val angle_loss = getPreferredAngle(link).map { preferred =>
        (angle - preferred).pow(2) * distance * angleWeight
      }.getOrElse(Value.zero)
      //println(s"${link.id} has angle ${angle} with loss ${angle_loss}")

      distance_loss + angle_loss
    }

    def adjust(stepSize: Double): Unit = {
      src.adjust(stepSize)
      dst.adjust(stepSize)
    }
  }

  def getPreferredDistance(state: ObjectState[LinkState]): Option[Int] = state.body.preferredDistance.collect {
    case 1 => 100
    case 2 => 150
    case 3 => 200
  }

  def getPreferredAngle(state: ObjectState[LinkState]): Option[Int] = state.body.preferredAngle.filter(a => a >= 0 && a < 360)
}
