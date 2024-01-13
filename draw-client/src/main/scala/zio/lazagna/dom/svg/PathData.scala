package zio.lazagna.dom.svg

sealed trait PathData {
  def render(buf: StringBuffer): Unit
}

object PathData {
  case class Move(dx: Double, dy: Double) extends PathData {
    override def render(buf: StringBuffer) = {
      buf.append("m ")
      buf.append(dx)
      buf.append(" ")
      buf.append(dy)
    }
  }
  case class MoveTo(x: Double, y: Double) extends PathData {
    override def render(buf: StringBuffer) = {
      buf.append("M ")
      buf.append(x)
      buf.append(" ")
      buf.append(y)
    }
  }
  case class Line(dx: Double, dy: Double) extends PathData {
    override def render(buf: StringBuffer) = {
      buf.append("l ")
      buf.append(dx)
      buf.append(" ")
      buf.append(dy)
    }
  }
  case class LineTo(x: Double, y: Double) extends PathData {
    override def render(buf: StringBuffer) = {
      buf.append("L ")
      buf.append(x)
      buf.append(" ")
      buf.append(y)
    }
  }
  case object Close extends PathData {
    override def render(buf: StringBuffer) = {
      buf.append("Z")
    }
  }

  def render(pathData: Iterable[PathData]): String = {
    val res = new StringBuffer
    pathData.foreach(_.render(res))
    res.toString()
  }
}
