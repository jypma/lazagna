package draw.client

import scala.collection.Searching.{Found, InsertionPoint}

import zio.stream.ZStream
import zio.{Hub, Ref, Semaphore, UIO, Scope, IO}
import zio.ZIO
import zio.lazagna.dom.indexeddb.IndexedDB
import zio.lazagna.dom.indexeddb.Schema
import zio.lazagna.dom.indexeddb.Schema.CreateObjectStore
import zio.lazagna.dom.indexeddb.IndexedDB.Blocked
import org.scalajs.dom.IDBTransactionMode
import org.scalajs.dom.IDBCursorDirection
import org.scalajs.dom.ErrorEvent
import org.scalajs.dom.DOMException
import zio.lazagna.dom.indexeddb.ObjectStore
import zio.lazagna.dom.indexeddb.Database
import scala.scalajs.js
import org.scalajs.dom.IDBKeyRange
import org.scalajs.dom
import zio.lazagna.Consumeable
import zio.lazagna.Consumeable.given

trait EventStore[E,Err] {
  def events: Consumeable[E] // No error type, the event stream should transparently reconnect, or inform the user if it's disconnected.
  def eventsAfter(sequenceNr: Long): Consumeable[E]
  def publish(event: E): IO[Err, Unit]
  def latestSequenceNr: ZIO[Any, Err, Long]
  def reset: UIO[Unit]
  def getSequenceNr(event: E): Long
}

object EventStore {
  /*
  // Based on  ZIO's SubscriptionRef, but keeping a history of values.
  def inMemory[E](_getSequenceNr: E => Long): UIO[EventStore[E, Nothing]] = {
    for {
      hub <- Hub.unbounded[E]
      existing <- Ref.make(Vector.empty[E])
      sequenceNrs <- Ref.make(Vector.empty[Long])
      semaphore <- Semaphore.make(1)
    } yield new EventStore[E, Nothing] {
      override def getSequenceNr(event: E) = _getSequenceNr(event)

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

      def latestSequenceNr: ZIO[Any, Nothing, Long] = sequenceNrs.get.map(_.lastOption.getOrElse(0))

      def start = ZIO.unit
    }
  }
  */

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
        println("prepend " + toPrepend.map(store.getSequenceNr) + " to " + sequenceNrs)
        val firstKnown = sequenceNrs.headOption
        val keep = toPrepend.takeWhile { e => !firstKnown.exists(_ <= store.getSequenceNr(e)) }
        State(
          events = keep.toVector ++ events,
          sequenceNrs = keep.view.map(store.getSequenceNr).toVector ++ sequenceNrs
        )
      }

      def publish(event: E): State = {
        println("publish " + store.getSequenceNr(event) + " to " + sequenceNrs)
        State(
        events = events :+ event,
        sequenceNrs = sequenceNrs :+ store.getSequenceNr(event)
      )}
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
                println(s"Serving from cache, ${selected.size} out of ${s.events.size} events.")
                // We have the first event still cached, no need to contact the store
                ZStream.fromHubScoped(hub).map { live =>
                  ZStream.fromIterable(selected) ++ live
                }
              } else {
                // Fetch missing events into our cache
                ZStream.fromHubScoped(hub).map { live =>
                  // get lastSequenceNr from store and fetch until there (or don't fetch if it's <= 0)
                  println(s"Fetching events from ${sequenceNr} until ${firstKnown}, we already have ${s.events.size}")
                  val early = ZStream.fromIterableZIO {
                    store.latestSequenceNr.flatMap { latestNr =>
                      store.eventsAfter(sequenceNr).takeWhile { e =>
                        !firstKnown.exists(_ <= getSequenceNr(e))
                      }
                        .takeUntil(e => getSequenceNr(e) >= latestNr)
                        .runCollect.tap { earlyEvents =>
                        println(s"Fetched ${earlyEvents.size}")
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
    }
  }

  trait Codec[T, U <: js.Any] {
    def encode(t: T): U
    def decode(u: U): T
  }

  type Err = DOMException | ErrorEvent
  def indexedDB[E,T <: js.Any](objectStoreName: String, _getSequenceNr: E => Long)(implicit codec: Codec[E,T]): ZIO[Database, Nothing, EventStore[E, Err]] = {
    // 1. Mount the render so it's subscribed to events (and will subscribe further events)
    // 2. Start reading from indexeddb into the hub (introduce a start() method)
    // 3. Continue real-time events into the hub
    // 4. eventsAfter disappears (since we only have all events, we don't re-fetch for now)

    for {
      db <- ZIO.service[Database]
      hub <- Hub.unbounded[E]
    } yield new EventStore[E, Err] {
      override def getSequenceNr(event: E) = _getSequenceNr(event)

      private def eventStoreRO: ZIO[Scope, Err, ObjectStore] = for {
        transaction <- db.transaction(Seq("events"))
        eventStore <- transaction.objectStore("events")
      } yield eventStore
      private def eventStoreRW: ZIO[Scope, Err, ObjectStore] = for {
        transaction <- db.transaction(Seq("events"), mode = IDBTransactionMode.readwrite)
        eventStore <- transaction.objectStore("events")
      } yield eventStore

      private def withEventStoreRO[U](zio: ObjectStore => ZIO[Scope, Err, U]): ZIO[Any, Err, U] =
        ZIO.scoped(eventStoreRO.flatMap(zio))
      private def withEventStoreRW[U](zio: ObjectStore => ZIO[Scope, Err, U]): ZIO[Any, Err, U] =
        ZIO.scoped(eventStoreRW.flatMap(zio))

      def events = eventsAfter(-1)
      //def events = hub

      def eventsAfter(afterSequenceNr: Long) = ZStream.unwrapScoped{
        println("eventsAfter " + afterSequenceNr)
        for {
          live <- ZStream.fromHubScoped(hub)
          latest <- getLastSequenceNr
          _ = println("Running eventsAfter from " +afterSequenceNr + " to " + latest)
          startTime = System.currentTimeMillis()
          range = try {
            if (afterSequenceNr == latest) {
              IDBKeyRange.bound(afterSequenceNr.toDouble, latest.toDouble, false, false)
            } else {
              IDBKeyRange.bound(afterSequenceNr.toDouble, latest.toDouble, true, false)
            }
          } catch { case x => x.printStackTrace(); null }
          arraybuffers <- db.getRange1("events", range).runCollect
          fetchTime = System.currentTimeMillis()
          events = arraybuffers.map { e =>
            codec.decode(e.asInstanceOf[T])
          }
          eventTime = System.currentTimeMillis()
          _ = println(s"Times: fetch ${fetchTime - startTime}ms, decode ${eventTime - fetchTime}ms")
          old = ZStream.fromIterable(events)
        } yield old ++ live.dropWhile { e => getSequenceNr(e) <= latest }
      }.catchAll { err =>
        dom.console.log(err)
        ZStream.empty
      }

      /*
      def start: ZIO[Scope, Nothing, Unit] = {
        for {
          latest <- getLastSequenceNr
          _ = println("Running eventsAfter to " + latest)
          range = IDBKeyRange.upperBound(latest.toDouble)
          startTime = System.currentTimeMillis()
          arraybuffers <- db.getRange1("events", range).runCollect
          fetchTime = System.currentTimeMillis()
          events = arraybuffers.map { e =>
            codec.decode(e.asInstanceOf[T])
          }
          eventTime = System.currentTimeMillis()
          _ = println(s"Times: fetch ${fetchTime - startTime}ms, decode ${eventTime - fetchTime}ms")
          _ <- ZStream.fromIterable(events).mapZIO(event => hub.publish(event)).runDrain.*>(started.set(true)).forkScoped
        } yield ()
      }.catchAllCause { err =>
        dom.console.log(err)
        ZIO.unit
      }
       */
      private def getLastSequenceNr = {
        for {
          latest <- db.getAll("events", IDBCursorDirection.prev).runHead
        } yield latest.map(e => getSequenceNr(codec.decode(e.asInstanceOf[T]))).getOrElse(0L)
      }.catchAll { err =>
        dom.console.log(err)
        ZIO.succeed(-1L)
      }
      def latestSequenceNr = getLastSequenceNr

      // FIXME: Only write to event store if we get a Web Lock
      // TODO: Add publish with type T directly (from websocket), so we can bypass codec there.
      def publish(event: E) = {
        db.add("events", codec.encode(event), getSequenceNr(event)) <* hub.publish(event)
      }

      def reset = db.clear("events").catchAll { err =>
        dom.console.log(err)
        ZIO.unit
      }
    }
  }
}
