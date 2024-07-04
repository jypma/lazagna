package zio.lazagna.geom

case class Point(x: Double, y: Double) {
  override def toString = f"($x%1.2f,$y%1.2f)"

  def move(deltaX: Double, deltaY: Double) = Point(x + deltaX, y + deltaY)
  def moveX(deltaX: Double) = Point(x + deltaX, y)
  def moveY(deltaY: Double) = Point(x, y + deltaY)
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

  def intersects(other: Line): Boolean = {
    val s10_x = to.x - from.x;
    val s10_y = to.y - from.y;
    val s32_x = other.to.x - other.from.x;
    val s32_y = other.to.y - other.from.y;

    val denom = s10_x * s32_y - s32_x * s10_y;
    if (denom == 0) /* Collinear */ false else {
      val denomPositive = denom > 0;

      val s02_x = from.x - other.from.x
      val s02_y = from.y - other.from.y
      val s_numer = s10_x * s02_y - s10_y * s02_x;
      if ((s_numer < 0) == denomPositive) false else {
        val t_numer = s32_x * s02_y - s32_y * s02_x;
        if ((t_numer < 0) == denomPositive) false else {
          if (((s_numer > denom) == denomPositive) || ((t_numer > denom) == denomPositive)) false else {
            // Collision detected
            /*
            t = t_numer / denom;
            if (i_x != NULL)
              *i_x = p0_x + (t * s10_x);
            if (i_y != NULL)
              *i_y = p0_y + (t * s10_y);
             */
            true
          }
        }
      }
    }
  }

  def intersects(rect: Rectangle): Boolean = {
    intersects(rect.leftBorder) || intersects(rect.topBorder) || intersects(rect.rightBorder) || intersects(rect.bottomBorder)
  }

  /** Returns the length of this line */
  def length: Double = Math.pow(Math.pow(to.x - from.x, 2) + Math.pow(to.y - from.y, 2), 0.5)

  /** Returns the angle of this line, in radians */
  def angle: Double = Math.atan2(to.y - from.y, to.x - from.x)
}

case class Bounds(width: Double, height: Double) {
  def at(p: Point): Rectangle = Rectangle(p, width, height)
  def at(x: Double, y: Double): Rectangle = Rectangle(Point(x,y), width, height)
  def middleAt(x: Double, y: Double): Rectangle = Rectangle(Point(x - 0.5 * width, y - 0.5 * height), width, height)
  def middleAt(p: Point): Rectangle = middleAt(p.x, p.y)
}

case class Rectangle(origin: Point, width: Double, height: Double) {
  override def toString = f"Rectangle($origin, $width%1.2fx$height%1.2f)"

  def moveTo(position: Point) = copy(origin = position)
  def move(deltaX: Double, deltaY: Double) = copy(origin = origin.move(deltaX, deltaY))

  def middle: Point = origin.move(width / 2, height / 2)
  def span: Point = origin.move(width, height)

  def leftBorder: Line = origin.to(origin.moveY(height))
  def topBorder: Line = origin.to(origin.moveX(width))
  def rightBorder: Line = {
    val topRight = origin.moveX(width)
    topRight.to(topRight.moveY(height))
  }
  def bottomBorder: Line = {
    val bottomLeft = origin.moveY(height)
    bottomLeft.to(bottomLeft.moveX(width))
  }

  def intersects(line: Line): Boolean = line.intersects(this)
  /** Expands the rectangle outwards by the given amount */
  def expand(delta: Double) = Rectangle(origin.move(-delta, -delta), width + 2 * delta, height + 2 * delta)
  /** Returns the largest rectangle including this and other */
  def union(other: Rectangle) = {
    val x1 = Math.min(origin.x, other.origin.x)
    val y1 = Math.min(origin.y, other.origin.y)
    val s = span
    val o = other.span
    val x2 = Math.max(s.x, o.x)
    val y2 = Math.max(s.y, o.y)
    Rectangle(Point(x1, y1), x2 - x1, y2 - y1)
  }
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
