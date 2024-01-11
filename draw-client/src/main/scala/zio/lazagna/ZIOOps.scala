package zio.lazagna

import zio.Hub
import zio.ZIO
import zio.Scope
import zio.Promise
import zio.stream.ZStream

object ZIOOps {
  /** Returns a ZIO that subscribes for elements on the given hub, and completes with Unit once subscription is set up. The fiber
    * reading elements is tied to a scope, and stops when that scope closes. */
  def consume[R,A](hub: Hub[A])(handle: A => ZIO[R,Nothing,Unit]): ZIO[R & Scope, Nothing, Unit] = {
    for {
      subscribed <- Promise.make[Nothing, Unit]
      in = ZStream.unwrapScoped(ZStream.fromHubScoped(hub).tap(_ => subscribed.succeed(())))
      _ <- in.mapZIO(handle).runDrain.forkScoped
      _ <- subscribed.await
    } yield ()
  }
}
