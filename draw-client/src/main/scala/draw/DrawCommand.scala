package draw

sealed trait DrawCommand

object DrawCommand {
  case class Point(x: Double, y: Double)

  case class StartScribble(id: Long, start: Point) extends DrawCommand
  case class ContinueScribble(id: Long, points: Seq[Point]) extends DrawCommand
  case class DeleteScribble(id: Long) extends DrawCommand

  case class Failed(message: String)
}
