package zio.lazagna

import zio.stream.ZStream
import zio.{Hub, Promise, Scope, ZIO}

/** A Consumeable is a ZStream that has extra Setup actions, and a Scope for background fibers. */
type Consumeable[T] = ZStream[Scope & Setup, Nothing, T]

object Consumeable {
  /** Constructs a Consumeable by reading from a hub */
  given fromHub[H]: Conversion[Hub[H], Consumeable[H]] = hub => ZStream.unwrapScoped(for {
    setup <- ZIO.service[Setup]
    subscribed <- Promise.make[Nothing, Unit]
    _ <- setup.addStartAction(subscribed.await)
    stream <- ZStream.fromHubScoped(hub).tap(_ => subscribed.succeed(()))
  } yield stream)

  extension(consumeable: Consumeable[_]) {
    /** Consumes everything of the given consumeable for its side effects only. */
    def consume: ZIO[Scope, Nothing, Unit] = {
      Setup.start {
        consumeable.runDrain.forkScoped.unit
      }
    }
  }
}
