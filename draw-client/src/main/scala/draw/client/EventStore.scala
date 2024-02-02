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

trait EventStore[T,E] {
  def events: ZStream[Any, E, T]
  def eventsAfter(sequenceNr: Long): ZStream[Any, E, T]
  def publish(event: T): IO[E, Unit]
  def latestSequenceNr: ZIO[Any, E, Long]
}

object EventStore {
  // Based on  ZIO's SubscriptionRef, but keeping a history of values.
  def inMemory[E](getSequenceNr: E => Long): UIO[EventStore[E, Nothing]] = {
    for {
      hub <- Hub.unbounded[E]
      existing <- Ref.make(Vector.empty[E])
      sequenceNrs <- Ref.make(Vector.empty[Long])
      semaphore <- Semaphore.make(1)
    } yield new EventStore[E, Nothing] {
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
    }
  }

  trait Codec[T, U <: js.Any] {
    def encode(t: T): U
    def decode(u: U): T
  }

  type Err = DOMException | ErrorEvent
  def indexedDB[E,T <: js.Any](objectStoreName: String, getSequenceNr: E => Long)(implicit codec: Codec[E,T]): ZIO[Database, Nothing, EventStore[E, Err]] = {
    // 1. Mount the render so it's subscribed to events (and will subscribe further events)
    // 2. Start reading from indexeddb into the hub (introduce a start() method)
    // 3. Continue real-time events into the hub
    // 4. eventsAfter disappears (since we only have all events, we don't re-fetch for now)

    for {
      db <- ZIO.service[Database]
      hub <- Hub.unbounded[E]
    } yield new EventStore[E, Err] {
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

      def events: zio.stream.ZStream[Any, Err, E] = eventsAfter(-1)

      def eventsAfter(afterSequenceNr: Long): zio.stream.ZStream[Any, Err, E] = ZStream.unwrapScoped{
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
        } yield old ++ live.dropWhile { e => getSequenceNr(e) <= afterSequenceNr }
      }

      private def getLastSequenceNr = {
        for {
          latest <- db.getAll("events", IDBCursorDirection.prev).runHead
        } yield latest.map(e => getSequenceNr(codec.decode(e.asInstanceOf[T]))).getOrElse(-1L)
      }
      def latestSequenceNr = getLastSequenceNr

      // FIXME: Only write to event store if we get a Web Lock
      // TODO: Add publish with type T directly (from websocket), so we can bypass codec there.
      def publish(event: E) = {
        db.add("events", codec.encode(event), getSequenceNr(event)) <* hub.publish(event)
      }
    }
  }
}
