package zio.lazagna

import zio.stream.{SubscriptionRef, ZStream}
import zio.{Dequeue, Hub, Promise, Scope, ZIO}

// TODO: Reconsider if Consumeable is allowed to have an error E, which we ignore when calling consume.

/** A Consumeable is a ZStream that has extra Setup actions, and a Scope for background fibers. */
type ConsumeableR[R,T] = ZStream[Scope & Setup & R, Nothing, T]
type Consumeable[T] = ZStream[Scope & Setup, Nothing, T]

object Consumeable {
  /** Constructs a Consumeable by reading from a hub */
  given fromHub[H]: Conversion[Hub[H], Consumeable[H]] = hub => ZStream.unwrapScoped(for {
    setup <- ZIO.service[Setup]
    subscribed <- Promise.make[Nothing, Unit]
    _ <- setup.addStartAction(subscribed.await)
    stream <- ZStream.fromHubScoped(hub).tap(_ => subscribed.succeed(()))
  } yield stream)

  given fromSubscriptionRef[T]: Conversion[SubscriptionRef[T], Consumeable[T]] = ref => ZStream.unwrapScoped(for {
    setup <- ZIO.service[Setup]
    subscribed <- Promise.make[Nothing, Unit]
    _ <- setup.addStartAction(subscribed.await)
  } yield ref.changes.tap(_ => subscribed.succeed(())))

  implicit def subscriptionRef2consumable[T](ref: SubscriptionRef[T]): Consumeable[T] = fromSubscriptionRef[T](ref)

  implicit def queue2consumeable[T](queue: Dequeue[T]): Consumeable[T] = ZStream.fromQueue(queue)

  implicit class ConsumeableOps[R,T](consumeable: ConsumeableR[R,T]) {
    /** Consumes everything of the given consumeable for its side effects only. */
    def consume: ZIO[Scope & R, Nothing, Unit] = {
      Setup.start[Scope & R, Nothing, Unit] {
        consumeable.runDrain.forkScoped.unit
      }
    }

    /** Consumes everything of the given consumeable for its side effects only, waiting for the first element to
      * be consumed before completing this ZIO. */
    def consumeAndAwaitFirst: ZIO[Scope & R, Nothing, Unit] = {
      Setup.start[Scope & R, Nothing, Unit] {
        Promise.make[Nothing, Unit].flatMap { promise =>
          consumeable.tap(_ => promise.completeWith(ZIO.unit)).runDrain.forkScoped.unit *> promise.await
        }
      }
    }

  }
}
