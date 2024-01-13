package zio.lazagna

import zio.Hub
import zio.ZIO
import zio.stream.ZStream
import zio.Scope
import zio.Promise
import zio.Trace

case class Consumeable[H,T](private val hub: Hub[H], private val transform: ZStream[Any, Nothing, H] => ZStream[Any, Nothing, T]) {
  def apply[U](t: ZStream[Any, Nothing, T] => ZStream[Any, Nothing, U]) = copy(transform = transform.andThen(t))

  def consume: ZIO[Scope, Nothing, Unit] = {
    for {
      subscribed <- Promise.make[Nothing, Unit]
      in = ZStream.unwrapScoped(ZStream.fromHubScoped(hub).tap(_ => subscribed.succeed(())))
      _ <- transform(in).runDrain.forkScoped
      _ <- subscribed.await
    } yield ()
  }
}

object Consumeable {
  given fromHub[H]: Conversion[Hub[H], Consumeable[H,H]] = hub => Consumeable(hub, s => s)
}
