package zio.lazagna

import zio.ZIO

/** Indicates that a ZIO didn't yield a value, because its operation was cancelled by a .filter() call. */
sealed trait Filtered {}

object Filtered {
  case object instance extends Filtered

  implicit class zioOps[R,E,T](zio: ZIO[R,E,T]) {
    /** Runs the underlying zio, but after that always fails with Filtered */
    def drain: ZIO[R, E | Filtered, Nothing] = zio.flatMap(_ => ZIO.fail(instance))
    /** Filters the zio with the given predicate, failing with Filtered if it doesn't match */
    def filter(p: T => Boolean): ZIO[R, E | Filtered, T] = zio.filterOrFail(p)(instance)
    /** Filters the zio with the given predicate, failing with Filtered if it doesn't match */
    def filterZIO(p: T => ZIO[Any, Nothing, Boolean]): ZIO[R, E | Filtered, T] = zio.flatMap { value =>
      p(value).flatMap {
        case true => ZIO.succeed(value)
        case false => ZIO.fail(instance)
      }
    }
    /** Renamed from collect(), since .collect() is defined in ZIO has taking the error value as first argument. */
    def collectF[U](pf: PartialFunction[T,U]): ZIO[R, E | Filtered, U] = zio.collect(instance)(pf)
  }
}
