package draw.client

import zio.stream.ZStream
import zio.UIO
import zio.Hub
import zio.Ref
import zio.Semaphore
import scala.collection.Searching.InsertionPoint
import scala.collection.Searching.Found

trait EventStore[E] {
  def events: ZStream[Any, Nothing, E]
  def eventsAfter(sequenceNr: Long): ZStream[Any, Nothing, E]
  def publish(event: E): UIO[Unit]
  def deleteBefore(event: E): UIO[Unit]
  def deleteBefore(sequenceNr: Long): UIO[Unit]
}

object EventStore {
  // Based on  ZIO's SubscriptionRef, but keeping a history of values.
  def make[E](getSequenceNr: E => Long): UIO[EventStore[E]] = {
    for {
      hub <- Hub.unbounded[E]
      existing <- Ref.make(Vector.empty[E])
      sequenceNrs <- Ref.make(Vector.empty[Long])
      semaphore <- Semaphore.make(1)
    } yield new EventStore[E] {
      def events: ZStream[Any, Nothing, E] =
        ZStream.unwrapScoped {
          semaphore.withPermit {
            existing.get.flatMap { e =>
              ZStream.fromHubScoped(hub).map { stream =>
                ZStream.fromIterable(e) ++ stream
              }
            }
          }
        }

      def eventsAfter(sequenceNr: Long): ZStream[Any, Nothing, E] =
        ZStream.unwrapScoped {
          semaphore.withPermit {
            existing.get.flatMap { e =>
              sequenceNrs.get.flatMap { nrs =>
                val startIdx = nrs.search(sequenceNr) match {
                  case Found(idx) => idx + 1
                  case InsertionPoint(idx) => idx
                }
                ZStream.fromHubScoped(hub).map { stream =>
                  ZStream.fromIterable(e.drop(startIdx)) ++ stream
                }
              }
            }
          }
        }

      def deleteBefore(sequenceNr: Long): UIO[Unit] = {
        semaphore.withPermit {
          sequenceNrs.get.flatMap { nrs =>
            val count = nrs.search(sequenceNr) match {
              case Found(idx) => idx
              case InsertionPoint(idx) => idx
            }
            existing.update(_.drop(count)) <* sequenceNrs.update(_.drop(count))
          }
        }
      }

      def deleteBefore(event: E): UIO[Unit] = deleteBefore(getSequenceNr(event))

      def publish(event: E): UIO[Unit] = semaphore.withPermit {
        existing.update(_ :+ event) <* sequenceNrs.update(_ :+ getSequenceNr(event)) <* hub.publish(event)
      }
    }
  }
}
