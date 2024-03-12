package zio.lazagna

import zio.{Ref, UIO, ZIO, ZLayer}

// TODO: Maybe this should just be a seq of Promise[Unit] to wait for, rather than full-on ZIO units.
/** A Setup block contains extra start actions that should be waited after completing a ZIO.
  This is different from Scope, since we want to add background fibers to Scope, but setup actions that
  wait for their creation in a Setup, from the same ZIO. */
trait Setup {
  def addStartAction[R](action: ZIO[R & Setup, Nothing, Any]): ZIO[R, Nothing, Unit]
}

object Setup {
  /** A version of Setup that can actually be started as part of a block. */
  sealed trait Startable extends Setup {
    def start: UIO[Unit]
  }

  /** Creates a new Setup, whose start actions can be started */
  def make: UIO[Setup.Startable] = {
    for {
      actions <- Ref.make[Seq[UIO[Unit]]](Seq.empty)
    } yield new Setup.Startable {
      override def addStartAction[R](action: ZIO[R & Setup, Nothing, Any]) = {
        ZIO.environment[R].flatMap { env =>
          actions.update(_ :+ action.unit.provideEnvironment(env.add(this)))
        }
      }
      override def start = actions.getAndSet(Seq.empty).flatMap {
        case s if s.isEmpty => ZIO.unit
        case s => ZIO.collectAll(s) *> start
      }
    }
  }

  /** Runs the given ZIO block, calling its start actions before returning. */
  def start[R,E,T](zio: => ZIO[R & Setup,E,T]): ZIO[R,E,T] = for {
    setup <- Setup.make
    res <- zio.provideSome[R](ZLayer.succeed(setup))
    _ <- setup.start
  } yield res
}
