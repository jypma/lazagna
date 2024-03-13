package zio.lazagna

import zio.lazagna.dom.Modifier
import zio.stream.{SubscriptionRef, ZStream}
import zio.{Dequeue, Hub, Promise, Scope, ZIO}

// TODO: Reconsider if Consumeable is allowed to have an error E, which we ignore when calling consume.

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

  given fromSubscriptionRef[T]: Conversion[SubscriptionRef[T], Consumeable[T]] = ref => ZStream.unwrapScoped(for {
    setup <- ZIO.service[Setup]
    subscribed <- Promise.make[Nothing, Unit]
    _ <- setup.addStartAction(subscribed.await)
  } yield ref.changes.tap(_ => subscribed.succeed(())))

  implicit def subscriptionRef2consumable[T](ref: SubscriptionRef[T]): Consumeable[T] = fromSubscriptionRef[T](ref)

  implicit def queue2consumeable[T](queue: Dequeue[T]): Consumeable[T] = ZStream.fromQueue(queue)

  extension(consumeable: Consumeable[_]) {
    /** Consumes everything of the given consumeable for its side effects only. */
    def consume: ZIO[Scope, Nothing, Unit] = {
      Setup.start {
        consumeable.runDrain.forkScoped.unit
      }
    }
  }
}
