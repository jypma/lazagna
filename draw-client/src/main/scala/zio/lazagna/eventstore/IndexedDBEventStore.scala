package zio.lazagna.eventstore

import scala.scalajs.js

import zio.lazagna.dom.indexeddb.{Database, Range, ValueCodec}
import zio.stream.{SubscriptionRef, ZStream}
import zio.{Chunk, Hub, IO, Scope, ZIO}

import org.scalajs.dom

/** An EventStore implementation that stores its data in the browser's indexeddb store. */
object IndexedDBEventStore {
  type Err = dom.DOMException | dom.ErrorEvent

  /** Creates an indexeddb-backed event store.
    *  @param objectStoreName The name of the object store to use inside the database (it must have been created using a schema)
    *  @param haveLock Should indicate if this browser tab has a write lock on the database (see Lock.makeAndLockExclusively)
    *  @param getSequenceNr Function that returns a sequence number for the specific event type
    */
  def make[E,T <: js.Any](objectStoreName: String, haveLock: SubscriptionRef[Boolean], _getSequenceNr: E => Long)(using codec: ValueCodec[E,T]): ZIO[Database with Scope, Nothing, EventStore[E, Err]] = {
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
          latest <- objectStore.getAll(dom.IDBCursorDirection.prev).runHead
        } yield latest.map(e => getSequenceNr(e)).getOrElse(0L)
      }.catchAll { err =>
        dom.console.log(err)
        ZIO.succeed(-1L)
      }
      def latestSequenceNr = getLastSequenceNr

      // TODO: Add publish with type T directly (from websocket), so we can bypass codec there.
      // TEST: Allow publishing the same identical event twice (idempotency)
      def publish(event: E) = {
        publishOrVerify(event).whenZIO(haveLock.get) *> hub.publish(event).unit
      }

      private def publishOrVerify(event: E) = {
        objectStore.add(event, getSequenceNr(event)).catchAll {
          case err =>
             // Adding to object store failed. Let's try to read instead, to allow idempotency.
            objectStore.get(getSequenceNr(event)).flatMap { _ match {
              case existing if existing == event =>
                ZIO.unit
              case other =>
                ZIO.fail(err)
            }}
        }
      }

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
