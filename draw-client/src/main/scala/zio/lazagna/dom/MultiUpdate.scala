package zio.lazagna.dom

import zio.stream.ZPipeline
import zio.{Exit, Ref, Scope, ZIO, ZLayer}

import org.scalajs.dom

/** Allows multiple modifiers to be mounted sequentially as a ZPipeline, in order to perform other actions
  * after all mounting is complete (or updated). The sequence of modifiers is applied every time a new T
  * arrives in a stream. Their Scope lasts until the next value of T arrives.
  *
  * Besides allowing completion callbacks, this can also improve performance since you can update multiple
  * attributes from one stream, without having to run it twice.
  */
// TODO: Add a way to do .changes() here, after Modifier refactor
trait MultiUpdate[T] { self =>
  /** Adds the given modifier to this update, executing it whenever a T arrives at the pipeline. */
  def apply(fn: T => Modifier): Modifier

  /** Returns a pipeline that will execute the modifiers that are part of this update */
  def pipeline: ZPipeline[Scope, Nothing, T, T]
}

object MultiUpdate {
  private case class State[T](
    updates: Seq[(dom.Element, T => Modifier)] = Seq.empty,
    currentScope: Option[Scope.Closeable] = None
  ) {
    def add(parent: dom.Element, fn: T => Modifier) = copy(
      updates = updates :+ (parent, fn)
    )

    def mountAll(parentScope: Scope, t: T): ZIO[Any, Nothing, State[T]] = for {
      _ <- ZIO.collectAll(currentScope.map(_.close(Exit.unit)))
      newScope <- parentScope.fork
      layer = ZLayer.succeed(newScope)
      _ <- ZIO.collectAll(updates.map { (parent, fn) =>
        fn(t).mount(parent).provideLayer(layer)
      })
    } yield copy(
      currentScope = Some(newScope)
    )
  }

  def make[T] = for {
    state <- Ref.Synchronized.make(State[T]())
  } yield new MultiUpdate[T] {
    override def apply(fn: T => Modifier) = Modifier { parent =>
      state.update(_.add(parent, fn))
    }

    override def pipeline: ZPipeline[Scope, Nothing, T, T] = ZPipeline.fromFunction[Scope, Nothing, T, T](
      _.mapZIO(t =>
        ZIO.scope.flatMap(scope => state.updateZIO(_.mountAll(scope, t)).as(t))
      ))
  }
}
