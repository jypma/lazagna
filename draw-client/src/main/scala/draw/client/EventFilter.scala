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
  )

  def make[E](emit: E => ZIO[Any, Nothing, Unit])(isUpdate: (E,E) => Boolean) = for {
    scope <- ZIO.scope
    state <- Ref.make(State[E]())
    semaphore <- Semaphore.make(1)
  } yield new EventFilter[E] {
    def emitQueue: ZIO[Any, Nothing, Unit] = semaphore.withPermit {
      state.get.flatMap {
        case s@State(_, q) if q.isEmpty =>
          state.set(s.copy(busy = false))
        case s@State(_, q) =>
          var queue = q
          while (queue.size > 1 && isUpdate(queue(0), queue(1))) {
            //println("Filtering an event.")
            queue = queue.tail
          }
          // We need to re-fork on emit to release the semaphore.
          state.set(s.copy(queue = queue.tail)) <* (emit(queue.head) *> emitQueue).forkIn(scope)
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
