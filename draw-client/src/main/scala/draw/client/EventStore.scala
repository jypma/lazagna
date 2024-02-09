package draw.client

import scala.collection.Searching.{Found, InsertionPoint}

import zio.stream.ZStream
import zio.{Hub, Ref, Semaphore, UIO, IO}
import zio.ZIO
import org.scalajs.dom.IDBCursorDirection
import org.scalajs.dom.ErrorEvent
import org.scalajs.dom.DOMException
import zio.lazagna.dom.indexeddb.Database
import zio.lazagna.dom.indexeddb.ValueCodec
import zio.lazagna.dom.indexeddb.Range
import scala.scalajs.js
import org.scalajs.dom
import zio.lazagna.Consumeable
import zio.lazagna.dom.weblocks.Lock
import zio.Scope
import zio.Promise
import zio.durationInt
import zio.stream.SubscriptionRef
import zio.Chunk

trait EventStore[E,Err] {
  def events: Consumeable[E] // No error type, the event stream should transparently reconnect, or inform the user if it's disconnected.
  def eventsAfter(sequenceNr: Long): Consumeable[E]
  def publish(event: E): IO[Err, Unit]
  def publishAndReplace(event: E, oldSequenceNr: Long): IO[Err, Unit]
  def delete(sequenceNr: Long): IO[Err, Unit]
  def latestSequenceNr: ZIO[Any, Err, Long]
  def reset: UIO[Unit]
  def getSequenceNr(event: E): Long
}

object EventStore {
  def cached[E,Err](store: EventStore[E,Err]): UIO[EventStore[E,Err]] = {
    // TODO: Consider changing this to Chunk instead of Vector
    // TODO: Remove the sequence nr vector, it's simpler and not much slower to just call the method
    case class State(events: Vector[E] = Vector.empty, sequenceNrs: Vector[Long] = Vector.empty) {
      // TODO: Remove these checks after we have tests
      if (sequenceNrs.size != sequenceNrs.toSet.size) {
        dom.console.log("Warning: cached duplicate sequenceNrs: " + sequenceNrs)
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

      def publish(event: E): IO[Err, Unit] = semaphore.withPermit {
        state.update(_.publish(event)) <* store.publish(event) <* hub.publish(event)
      }

      def latestSequenceNr: ZIO[Any, Err, Long] = store.latestSequenceNr

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

  type Err = DOMException | ErrorEvent
  def indexedDB[E,T <: js.Any](objectStoreName: String, haveLock: SubscriptionRef[Boolean], _getSequenceNr: E => Long)(using codec: ValueCodec[E,T]): ZIO[Database with Scope, Nothing, EventStore[E, Err]] = {
    for {
      db <- ZIO.service[Database]
      objectStore = db.objectStore[E,T,Long](objectStoreName)
      hub <- Hub.unbounded[E]
    } yield new EventStore[E, Err] {
      override def getSequenceNr(event: E) = _getSequenceNr(event)

      def events = eventsAfter(-1)

      def eventsAfter(afterSequenceNr: Long) = ZStream.unwrapScoped{
        for {
          live <- ZStream.fromHubScoped(hub)
          latest <- getLastSequenceNr
          events <- if (afterSequenceNr == latest) ZIO.succeed(Chunk.empty) else {
            val range = Range.bound(afterSequenceNr, latest, true, false)
            objectStore.getRange(range).runCollect
          }
          old = ZStream.fromIterable(events)
        } yield old ++ live.dropWhile { e => getSequenceNr(e) <= latest }
      }.catchAll { err =>
        dom.console.log(err)
        ZStream.empty
      }

      private def getLastSequenceNr = {
        for {
          latest <- objectStore.getAll(IDBCursorDirection.prev).runHead
        } yield latest.map(e => getSequenceNr(e)).getOrElse(0L)
      }.catchAll { err =>
        dom.console.log(err)
        ZIO.succeed(-1L)
      }
      def latestSequenceNr = getLastSequenceNr

      // TODO: Add publish with type T directly (from websocket), so we can bypass codec there.
      def publish(event: E) = objectStore.add(event, getSequenceNr(event)).whenZIO(haveLock.get) *> hub.publish(event).unit

      def reset = objectStore.clear.whenZIO(haveLock.get).unit.catchAll { err =>
        dom.console.log(err)
        ZIO.unit
      }

      def publishAndReplace(event: E, oldSequenceNr: Long): IO[Err, Unit] = {
        objectStore.add(event, getSequenceNr(event)) *> objectStore.delete(oldSequenceNr)
      }.whenZIO(haveLock.get) *> hub.publish(event).unit

      def delete(sequenceNr: Long): IO[Err, Unit] = objectStore.delete(sequenceNr).whenZIO(haveLock.get).unit
    }
  }
}
