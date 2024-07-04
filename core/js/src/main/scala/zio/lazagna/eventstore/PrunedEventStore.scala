package zio.lazagna.eventstore

import zio.lazagna.{Consumeable, Setup}
import zio.stream.{SubscriptionRef, ZStream}
import zio.{IO, Scope, ZIO, ZLayer}

import izumi.reflect.Tag
import org.scalajs.dom

/** Provides an alternative, more compact copy of an event store's events, where events can be "pruned" and
  * selectively merged, or even deleted. This is useful to allow much faster rematerialization of events, and
  * a more simple alternative to using snapshots. While materializing, a state S is kept in memory.
  *
  *  @param source The actual source of underlying events
  *  @param storage Target storage to store pruned events into
  *  @param haveLock Should indicate if this browser tab has a write lock on the database (see Lock.makeAndLockExclusively)
  *  @param initialState In-memory state to start with (before any events have been read)
  *  @param prune Updates the in-memory state, and potentially updates target storage, as a result to an
  *         incoming event from [source]. The resulting ZIO's environment will contain a reference to the target
  *         storage.
  *  @param recover Updates the in-memory state as a result of an event that was earlier pruned into [storage]
  */
object PrunedEventStore {
  def make[E: Tag, Err: Tag, S](source: EventStore[E,Err], storage: EventStore[E,Err], haveLock: SubscriptionRef[Boolean], initialState: S)
    (prune: (S, E) => ZIO[EventStore[E,Err], Err, S])
    (recover: (S, E) => S)
      : ZIO[Scope & Setup, Nothing, EventStore[E,Err]] = {

    // Materialize incoming events from [source], prune them, and store them into storage.
    val materialize = for {
      latestSeqNr <- storage.latestSequenceNr
      initialState <- if (latestSeqNr > 0) {
        storage.events.takeUntil(e => storage.getSequenceNr(e) >= latestSeqNr).runFold(initialState)(recover)
      } else {
        ZIO.succeed(initialState)
      }
      _ <- source.eventsAfter(latestSeqNr).runFoldZIO(initialState)(prune(_,_).provide(ZLayer.succeed(storage))).catchAll { err =>
        dom.console.log(err)
        ZIO.unit
      }
    } yield ()

    for {
      // We only materialize if we have the write lock (so we only write from one browser tab at a time)
      setup <- ZIO.service[Setup]
      _ <- setup.addStartAction {
        haveLock.changes.filter(_ == true).take(1).mapZIO(_ => materialize).runDrain.forkScoped
      }
    } yield new EventStore[E,Err] {
      def events: Consumeable[E] = ZStream.unwrap {
        // read from storage until latest there, read rest from source
        for {
          latest <- storage.latestSequenceNr
          fromStorage = storage.events.takeUntil(e => storage.getSequenceNr(e) >= latest)
          fromSource = source.eventsAfter(latest)
        } yield fromStorage ++ fromSource
      }.catchAll { e =>
        dom.console.log(e)
        ZStream.empty
      }

      def eventsAfter(sequenceNr: Long): Consumeable[E] = ZStream.unwrap {
        // read from storage until latest there, read rest from source
        for {
          latest <- storage.latestSequenceNr
          fromStorage = if (latest == 0 || sequenceNr > latest) {
            ZStream.empty
          } else storage.eventsAfter(sequenceNr).takeUntil(e => storage.getSequenceNr(e) >= latest)
          fromSource = source.eventsAfter(Math.max(latest, sequenceNr))
        } yield fromStorage ++ fromSource
      }.catchAll { e =>
        dom.console.log(e)
        ZStream.empty
      }

      def publish(event: E): IO[Err, Unit] = {
        // just forward, it'll come back to be materialized
        source.publish(event)
      }

      def publishAndReplace(event: E, oldSequenceNr: Long): IO[Err, Unit] ={
        // delete locally and forward
        storage.delete(oldSequenceNr) *> source.publish(event)
      }

      def delete(sequenceNr: Long): IO[Err, Unit] = {
        // won't be called... delete locally and forward
        storage.delete(sequenceNr) *> source.delete(sequenceNr)
      }

      // TEST: Don't show event numbers for deleted events that are most recent.
      // We read this from the pruned events store, since those are the only events the client is going to see.
      def latestSequenceNr: IO[Err, Long] = storage.latestSequenceNr

      def reset: IO[Err, Unit] = storage.reset *> source.reset  // reset both

      def getSequenceNr(event: E): Long = source.getSequenceNr(event)
    }
  }

}
