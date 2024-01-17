package zio.lazagna

import zio.stream.ZStream
import zio.{Hub, Promise, Scope, ZIO}

/** Represents something that can potentially create a stream of items T when started in a scope. */
trait Consumeable[T] {
  /** Applies the given transformation to the stream, once it starts */
  def apply[U](t: ZStream[Any, Nothing, T] => ZStream[Any, Nothing, U]): Consumeable[U]

  private[lazagna] def stream(onStart: => ZIO[Any, Nothing, Any]): ZStream[Any, Nothing, T]

  /** Returns a ZIO that, when started, runs this consumable and its transformations */
  def consume: ZIO[Scope, Nothing, Unit] = {
    for {
      subscribed <- Promise.make[Nothing, Unit]
      _ <- stream(subscribed.succeed(())).runDrain.forkScoped
      _ <- subscribed.await
    } yield ()
  }

  /** Returns a new Consumeable that emits values to its stream when either this or the other emit. */
  def merge[T1 <: T](other: Consumeable[T1]) = MergedConsumeable(this, other, s => s)
}

case class HubConsumeable[H,T](private val hub: Hub[H], private val transform: ZStream[Any, Nothing, H] => ZStream[Any, Nothing, T]) extends Consumeable[T] {
  override def apply[U](t: ZStream[Any, Nothing, T] => ZStream[Any, Nothing, U]) = copy(transform = transform.andThen(t))

  override def stream(onStart: => ZIO[Any, Nothing, Any]): ZStream[Any, Nothing, T] = {
    val in = ZStream.unwrapScoped(ZStream.fromHubScoped(hub).tap(_ => onStart))
    transform(in)
  }

}

case class MergedConsumeable[H1,H2 <: H1,T](a: Consumeable[H1], b: Consumeable[H2], transform: ZStream[Any, Nothing, H1] => ZStream[Any, Nothing, T]) extends Consumeable[T] {
  override def apply[U](t: ZStream[Any, Nothing, T] => ZStream[Any, Nothing, U]) = copy(transform = transform.andThen(t))

  override def stream(onStart: => ZIO[Any, Nothing, Any]): ZStream[Any, Nothing, T] = {
    ZStream.unwrapScoped(for {
      subscribedA <- Promise.make[Nothing, Unit]
      subscribedB <- Promise.make[Nothing, Unit]
      checkStarted = subscribedA.poll.zip(subscribedB.poll).flatMap {
        case (Some(_), Some(_)) => onStart
        case _ => ZIO.unit
      }
      inA = a.stream(subscribedA.succeed(()) *> checkStarted)
      inB = b.stream(subscribedB.succeed(()) *> checkStarted)
    } yield {
      transform(inA.merge(inB))
    })
  }
}

object Consumeable {
  given fromHub[H]: Conversion[Hub[H], Consumeable[H]] = hub => HubConsumeable(hub, s => s)
}
