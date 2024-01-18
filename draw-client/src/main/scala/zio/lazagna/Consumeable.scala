package zio.lazagna

import zio.stream.ZStream
import zio.{Hub, Promise, Scope, ZIO}

/** A Consumeable is a ZStream that has extra Setup actions, and a Scope for background fibers. */
type Consumeable[T] = ZStream[Scope & Setup, Nothing, T]

object Consumeable {
  /** Constructs a Consumeable by reading from a hub */
  given fromHub[H]: Conversion[Hub[H], Consumeable[H]] = hub => ZStream.unwrap(for {
    setup <- ZIO.service[Setup]
    subscribed <- Promise.make[Nothing, Unit]
    _ <- setup.addStartAction(subscribed.await)
    zioOfStream <- ZStream.fromHubScoped(hub).tap(_ => subscribed.succeed(()))
  } yield zioOfStream)

  /** Consumes everything of the given consumeable for its side effects only. */
  def consume(consumeable: Consumeable[_]): ZIO[Scope, Nothing, Unit] = {
    Setup.start {
      consumeable.runDrain.forkScoped.unit
    }
  }
}
