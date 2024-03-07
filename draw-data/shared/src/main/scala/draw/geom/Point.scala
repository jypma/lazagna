package draw.geom

case class Point(x: Double, y: Double) {
  def move(deltaX: Double, deltaY: Double) = Point(x + deltaX, y + deltaY)
  def move(delta: Point) = Point(x + delta.x, y + delta.y)
  def to(p: Point) = Line(this, p)

  /** Returns a line from here to the middle of the given rectangle, stopping at the intersection of
    * the rectangle's boundary. */
  def toIntersection(r: Rectangle): Line = {
    val min = r.origin
    val mid = r.middle
    val max = r.span

    val m = (mid.y - y) / (mid.x - x)

    val minXy = m * (min.x - x) + y
    val maxXy = m * (max.x - x) + y
    val minYx = (min.y - y) / m + x
    val maxYx = (max.y - y) / m + x

    val to = if ((x <= mid.x) && (min.y <= minXy && minXy <= max.y)) {
      Point(min.x, minXy)
    } else if ((x >= mid.x) && (min.y <= maxXy && maxXy <= max.y)) {
      Point(max.x, maxXy)
    } else if ((y <= mid.y) && (min.x <= minYx && minYx <= max.x)) {
      Point(minYx, min.y)
    } else if ((y >= mid.y) && (min.x <= maxYx && maxYx <= max.x)) {
      Point(maxYx, max.y)
    } else {
      this
    }

    Line(this, to)
  }
}

case class Line(from: Point, to: Point) {
  def reverse = Line(to, from)

  def intersects(rect: Rectangle): Boolean = {
    def quadrant(p: Point) = {
      if (p.x < rect.origin.x) {
        if (p.y < rect.origin.y) "1001".b
        else if (p.y > rect.origin.y + rect.height) "1010".b
        else "1000".b
      } else if (p.x > rect.origin.x + rect.width) {
        if (p.y < rect.origin.y) "0101".b
        else if (p.y > rect.origin.y + rect.height) "0110".b
        else "0100".b
      } else {
        if (p.y < rect.origin.y) "0001".b
        else if (p.y > rect.origin.y + rect.height) "0010".b
        else "0000".b
      }
    }

    (quadrant(from) & quadrant(to)) == 0
  }
}

case class Rectangle(origin: Point, width: Double, height: Double) {
  def middle: Point = origin.move(width / 2, height / 2)
  def span: Point = origin.move(width, height)

  def intersects(line: Line): Boolean = line.intersects(this)
  def expand(delta: Double) = Rectangle(origin.move(-delta, -delta), width + 2 * delta, height + 2 * delta)
}

object Rectangle {
  def apply(p1: Point, p2: Point): Rectangle = {
    val x = Math.min(p1.x, p2.x)
    val y = Math.min(p1.y, p2.y)
    val w = Math.abs(p2.x - p1.x)
    val h = Math.abs(p2.y - p1.y)
    Rectangle(Point(x, y), w, h)
  }
}

private[geom] implicit class IntToBase( val digits:String ) extends AnyVal {
  private def base(b:Int) = Integer.parseInt( digits, b )
  def b = base(2)
  def o = base(8)
  def x = base(16)
}
