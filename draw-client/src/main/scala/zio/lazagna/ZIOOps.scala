package zio.lazagna

import zio.Hub
import zio.ZIO
import zio.Scope
import zio.Promise
import zio.stream.ZStream

object ZIOOps {
  /** Returns a ZIO that subscribes for elements on the given hub, and completes with Unit once subscription is set up. The fiber
    * reading elements is tied to a scope, and stops when that scope closes. */
  def consumeWith[R,T](hub: Hub[T])(handle: ZStream[Any,Nothing,T] => ZStream[R,Nothing,Unit]): ZIO[R & Scope, Nothing, Unit] = {
    for {
      subscribed <- Promise.make[Nothing, Unit]
      in = ZStream.unwrapScoped(ZStream.fromHubScoped(hub).tap(_ => subscribed.succeed(())))
      _ <- handle(in).runDrain.forkScoped
      _ <- subscribed.await
    } yield ()
  }

  def consume[R,H,T](consumeable: Consumeable[H,T])(handle: T => ZIO[R,Nothing,Unit]): ZIO[R & Scope, Nothing, Unit] = {
    consumeWith(consumeable.hub)(_.mapZIO(consumeable.transformElement).mapZIO(handle))
  }

  def consume[R,T](hub: Hub[T])(handle: T => ZIO[R,Nothing,Unit]): ZIO[R & Scope, Nothing, Unit] = {
    consumeWith(hub)(_.mapZIO(handle))
  }
}
