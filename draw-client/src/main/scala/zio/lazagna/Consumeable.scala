package zio.lazagna

import zio.Hub
import zio.ZIO

case class Consumeable[H,T](hub: Hub[H], transformElement: H => ZIO[Any, Nothing, T]) {
  def map[U](fn: T => U) = copy(transformElement = transformElement.andThen(_.map(fn)))
}

object Consumeable {
  given fromHub[H]: Conversion[Hub[H], Consumeable[H,H]] = hub => Consumeable(hub, ZIO.succeed _)
}
