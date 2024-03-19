package draw.client

import zio.ZIO
import zio.Ref
import zio.Semaphore
import zio.Chunk

trait EventFilter[E] {
  def publish(event: E): ZIO[Any, Nothing, Unit]
}

object EventFilter {
  private case class State[E](
    busy: Boolean = false,
    queue: Chunk[E] = Chunk.empty
  ) {
    // precondition: queue is not empty
    def mergeAndEmit(merge: (E,E) => Option[E]): (State[E], E) = {
      if (queue.size == 1) {
        (copy(queue = Chunk.empty), queue.head)
      } else merge(queue(0), queue(1)) match {
        case None =>
          (copy(queue = queue.tail), queue.head)
        case Some(merged) =>
          println("Merged an event.")
          copy(queue = merged +: queue.drop(2)).mergeAndEmit(merge)
      }
    }
  }

  def make[E](emit: E => ZIO[Any, Nothing, Unit])(mergeFn: (E,E) => Option[E]) = for {
    scope <- ZIO.scope
    state <- Ref.make(State[E]())
    semaphore <- Semaphore.make(1)
  } yield new EventFilter[E] {
    def emitQueue: ZIO[Any, Nothing, Unit] = semaphore.withPermit {
      state.get.flatMap {
        case s@State(_, q) if q.isEmpty =>
          state.set(s.copy(busy = false))
        case s =>
          val (newState, e) = s.mergeAndEmit(mergeFn)
          // We need to re-fork on emit to release the semaphore.
          state.set(newState) <* (emit(e) *> emitQueue).forkIn(scope)
      }
    }

    def publish(event: E) = semaphore.withPermit {
      state.get.flatMap {
        case s@State(false, _) =>
          (state.set(s.copy(busy = true)) <* (emit(event) *> emitQueue).forkIn(scope))
        case s@State(true, _) =>
          state.set(s.copy(queue = s.queue :+ event))
      }
    }
  }
}
