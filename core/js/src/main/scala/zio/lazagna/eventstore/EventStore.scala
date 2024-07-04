package zio.lazagna.eventstore

import zio.IO
import zio.lazagna.Consumeable

/** A persistent container for an event-sourced collection of events. Events have a sequence number that
  * provides an ordering for them. */
trait EventStore[E,Err] {
  /** Returns all events in the event store as a stream. This will return events already in the events first,
    * and then continue delivering real-time events as they are published. This has no error type, the event
    * stream should transparently reconnect, or inform the user out of bounds if it's disconnected. */
  def events: Consumeable[E]

  /** Returns events that take place AFTER the given sequence number as a stream. This will return events
    * already in the events first, and then continue delivering real-time events as they are published. This
    * has no error type, the event stream should transparently reconnect, or inform the user out of bounds if
    * it's disconnected. */
  def eventsAfter(sequenceNr: Long): Consumeable[E]

  /** Publishes the given event to the store (and emitting it on any ongoing event streams). The event MUST have
    * a sequence number higher than any other already published events. */
  def publish(event: E): IO[Err, Unit]

  /** Publishes the given event to the store (and emitting it on any ongoing event streams). Any existing event
    * with the given oldSequenceNr is deleted. The new event MUST have a sequence number higher than any other
    * already published events. */
  def publishAndReplace(event: E, oldSequenceNr: Long): IO[Err, Unit]

  /** Deletes any event with the given sequence number. */
  def delete(sequenceNr: Long): IO[Err, Unit]

  /** Returns the highest event sequence number that is currently in the store. */
  def latestSequenceNr: IO[Err, Long]

  /** Removes all events from the store */
  def reset: IO[Err, Unit]

  /** Returns the sequence number for the given event */
  def getSequenceNr(event: E): Long
}
