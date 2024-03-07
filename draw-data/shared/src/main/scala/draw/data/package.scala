package draw

package object data {
  implicit def toProtobuf(p: draw.geom.Point): draw.data.point.Point = draw.data.point.Point(p.x, p.y)
  implicit def fromProtobuf(p: draw.data.point.Point): draw.geom.Point = draw.geom.Point(p.x, p.y)
}
