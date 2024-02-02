package zio.lazagna.dom.weblocks

import scala.scalajs.js
import org.scalajs.dom
import zio.ZIO
import zio.Scope

/** A wrapper around the Web Locks API */
trait Lock {
  def withExclusiveLock[R,E,A](zio: =>ZIO[R,E,A]): ZIO[R,E,A]
  def withExclusiveLockScoped: ZIO[Scope, Nothing, Unit]
}

private[weblocks] case class LockImpl(name: String) extends Lock {
  def withExclusiveLockScoped: ZIO[Scope, Nothing, Unit] = {
    ZIO.acquireRelease {
      var release: ZIO[Any, Nothing, Unit] = null
      ZIO.async[Any, Nothing, ZIO[Any, Nothing, Unit]] { cb =>
        dom.window.navigator.locks.request(name, { lock =>
          // We have the lock here!
          new js.Promise[Unit]((resolve, reject) => {
            // Cancel the lock by calling resolve: this will be our release action.
            cb(ZIO.succeed(ZIO.succeed(resolve(()))))
          })
        })
      }
    } { r => r }
  }.unit

  def withExclusiveLock[R,E,A](zio: =>ZIO[R,E,A]): ZIO[R,E,A] = {
    ZIO.scoped(withExclusiveLockScoped *> zio)
  }
}

object Lock {
  /** Makes a lock with the given name. */
  def make(name: String): Lock = LockImpl(name)
}
