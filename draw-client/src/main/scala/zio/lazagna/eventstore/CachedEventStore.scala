package zio.lazagna.eventstore

import scala.collection.Searching.{Found, InsertionPoint}

import zio.stream.ZStream
import zio.{Hub, IO, Ref, Semaphore, UIO}

import org.scalajs.dom
import zio.ZIO


/** An EventStore implementation that caches events in memory. */
object CachedEventStore {
  /** Returns an EventStore that caches events in memory. */
  def make[E,Err](store: EventStore[E,Err]): UIO[EventStore[E,Err]] = {
    // TODO: Create a bound on the number of events to cache
    // TODO: Consider changing this to Chunk instead of Vector
    // TODO: Remove the sequence nr vector, it's simpler and not much slower to just call the method
    case class State(events: Vector[E] = Vector.empty, sequenceNrs: Vector[Long] = Vector.empty) {
      // TODO: Remove these checks after we have tests
      if (sequenceNrs.size != sequenceNrs.toSet.size) {
        dom.console.log("Warning: cached duplicate sequenceNrs: " + sequenceNrs)
        events.groupBy(store.getSequenceNr).filter(_._2.size > 1).foreach { (nr, events) =>
          println(s"${nr}:")
          for (e <- events) {
            println(s"  ${e}")
          }
        }
      }
      if (sequenceNrs.sorted != sequenceNrs) {
        dom.console.log("Warning: sequenceNrs not sorted: " + sequenceNrs)
      }

      def prepend(toPrepend: Iterable[E]): State = {
        val firstKnown = sequenceNrs.headOption
        val keep = toPrepend.takeWhile { e => !firstKnown.exists(_ <= store.getSequenceNr(e)) }
        State(
          events = keep.toVector ++ events,
          sequenceNrs = keep.view.map(store.getSequenceNr).toVector ++ sequenceNrs
        )
      }

      // TEST: Verify that idempotency works here
      def publish(event: E): State = {
        State(
          events = events :+ event,
          sequenceNrs = sequenceNrs :+ store.getSequenceNr(event)
        )
      }

      def delete(sequenceNr: Long) = {
        val idx = sequenceNrs.search(sequenceNr) match {
          case Found(i) => i
          case _ => -1
        }
        copy(
          events = events.patch(idx, Seq.empty, if (idx == -1) 0 else 1),
          sequenceNrs = sequenceNrs.patch(idx, Seq.empty, if (idx == -1) 0 else 1)
        )
      }
    }

    for {
      hub <- Hub.unbounded[E]
      state <- Ref.make(State())
      semaphore <- Semaphore.make(1)
    } yield new EventStore[E,Err] {
      override def getSequenceNr(event: E) = store.getSequenceNr(event)

      def eventsAfter(sequenceNr: Long) =
        ZStream.unwrapScoped {
          semaphore.withPermit {
            state.get.flatMap { s =>
              val firstKnown = s.sequenceNrs.headOption
              val startIdx = s.sequenceNrs.search(sequenceNr) match {
                case Found(idx) => idx + 1
                case InsertionPoint(idx) => idx
              }
              if (startIdx > 0 || firstKnown.contains(sequenceNr)) {
                val selected = s.events.drop(startIdx)
                // We have the first event still cached, no need to contact the store
                ZStream.fromHubScoped(hub).map { live =>
                  ZStream.fromIterable(selected) ++ live
                }
              } else {
                // Fetch missing events into our cache
                ZStream.fromHubScoped(hub).map { live =>
                  // get lastSequenceNr from store and fetch until there (or don't fetch if it's <= 0)
                  val early = ZStream.fromIterableZIO {
                    store.latestSequenceNr.flatMap { latestNr =>
                      store.eventsAfter(sequenceNr).takeWhile { e =>
                        !firstKnown.exists(_ <= getSequenceNr(e))
                      }
                        .takeUntil(e => getSequenceNr(e) >= latestNr)
                        .runCollect.tap { earlyEvents =>
                          state.update(_.prepend(earlyEvents))
                        }
                    }
                  }
                  early ++ ZStream.fromIterable(s.events.drop(startIdx)) ++ live
                }
              }
            }
          }
        }.catchAll { err =>
          dom.console.log(err)
          ZStream.empty
        }

      def events = eventsAfter(0)

      def publish(event: E): IO[Err, Unit] = {
        semaphore.withPermit {
          state.update(_.publish(event)) *> store.publish(event) *> hub.publish(event).unit
        }
      }

      def latestSequenceNr: IO[Err, Long] = store.latestSequenceNr

      def reset = semaphore.withPermit {
        state.set(State()) *> store.reset
      }

      def publishAndReplace(event: E, oldSequenceNr: Long) = semaphore.withPermit {
        state.update(_.delete(oldSequenceNr).publish(event)) <* store.publishAndReplace(event, oldSequenceNr) <* hub.publish(event)
      }

      def delete(sequenceNr: Long): IO[Err, Unit] = semaphore.withPermit {
        state.update(_.delete(sequenceNr)) *> store.delete(sequenceNr)
      }
    }
  }
}
