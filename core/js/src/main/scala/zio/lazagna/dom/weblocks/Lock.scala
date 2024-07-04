package zio.lazagna.dom.weblocks

import scala.scalajs.js

import zio.stream.SubscriptionRef
import zio.{Promise, Scope, UIO, ZIO, durationInt}

import org.scalajs.dom

import Lock._

/** A wrapper around the Web Locks API */
trait Lock {
  private[Lock] def doLock(mode: dom.LockMode = dom.LockMode.exclusive, ifAvailable: Boolean = false, steal: Boolean = false): ZIO[Scope, LockFailed, Unit]

  def withExclusiveLock[R,E,A](zio: =>ZIO[R,E,A]): ZIO[R,E,A] = {
    ZIO.scoped(doLock().catchAll { _ => ZIO.unit } *> zio)
  }

  def withExclusiveLockIfAvailable[R,E,A](zio: =>ZIO[R,E,A]): ZIO[R, E | LockUnavailable.type, A] = {
    ZIO.scoped(doLock(ifAvailable = true).catchAll {
      case LockUnavailable => ZIO.fail(LockUnavailable)
    } *> zio)
  }
}

private[weblocks] case class LockImpl(name: String) extends Lock {
  def doLock (mode: dom.LockMode = dom.LockMode.exclusive, ifAvailable: Boolean = false, steal: Boolean = false): ZIO[Scope, LockFailed, Unit] = {
    ZIO.acquireRelease {
      ZIO.asyncInterrupt[Any, LockFailed, UIO[Unit]] { cb =>
        try {
          val controller = new dom.AbortController()

          val options = new dom.LockOptions {}
          options.mode = mode
          options.ifAvailable = ifAvailable
          options.steal = steal
          if (!ifAvailable) {
            options.signal = controller.signal
          }

          dom.window.navigator.locks.request(name, options, { lock =>
            if (lock != null) {
              // We have the lock here!
              new js.Promise[Unit]((resolve, reject) => {
                // Cancel the lock by calling resolve: this will be our release action.
                cb(ZIO.succeed(ZIO.succeed(resolve(()))))
              })
            } else {
              // When called with ifAvailable = true, and the lock wasn't available, we didn't get it.
              cb(ZIO.fail(LockUnavailable))
              new js.Promise[Unit]((resolve, reject) => {})
            }
          })

          Left(ZIO.succeed(controller.abort()))
        } catch {
          case x =>
            x.printStackTrace()
            Left(ZIO.unit)
        }
        // Abort the lock acquisition if the fiber is interrupted
      }
    } { r => r }
  }.unit
}

object Lock {
  sealed trait LockFailed
  case object LockUnavailable extends LockFailed

  /** Makes a lock with the given name. */
  def make(name: String): UIO[Lock] = ZIO.succeed(LockImpl(name))

  /** Make a lock and try to acquire it under the given scope in a forked fiber, reflecting the current lock
    * state in the returned Ref. If the lock is available at creation time, this tries to wait for the lock to
    * actually be acquired. */
  def makeAndLockExclusively(name: String): ZIO[Scope, Nothing, SubscriptionRef[Boolean]] = for {
    lock <- Lock.make(name)
    haveLock <- SubscriptionRef.make(false)
    lockInitiallyAvailable <- lock.withExclusiveLockIfAvailable(ZIO.succeed(true)).catchAll { _ => ZIO.succeed(false) }
    promise <- Promise.make[Nothing, Unit]
    start = System.currentTimeMillis()
    _ <- lock.withExclusiveLock(
      haveLock.set(true) *>
        promise.completeWith(ZIO.unit) *>
        ZIO.succeed(println(s"Lock acquired after ${System.currentTimeMillis - start}ms!")) *>
        ZIO.never
    ).forkScoped
    _ <- if (lockInitiallyAvailable) {
      // Acquire lock, and wait up to 5 seconds for it to actually be acquired
      promise.await.timeout(5.seconds)
    } else {
      // Acquire lock, don't wait
      println("Couldn't get lock, waiting for it.")
      ZIO.unit
    }
  } yield haveLock
}
