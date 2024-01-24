package zio.lazagna

import zio.{Ref, UIO, ZIO, ZLayer}

// TODO: Maybe this should just be a seq of Promise[Unit] to wait for, rather than full-on ZIO units.
/** A Setup block contains extra start actions that should be waited after completing a ZIO.
  This is different from Scope, since we want to add background fibers to Scope, but setup actions that
  wait for their creation in a Setup, from the same ZIO. */
trait Setup {
  def addStartAction(action: UIO[Unit]): UIO[Unit]
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
      override def addStartAction(action: UIO[Unit]) = actions.update(_ :+ action)
      override def start = actions.get.flatMap(ZIO.collectAll).unit
    }
  }

  /** Runs the given ZIO block, calling its start actions before returning. */
  def start[R,E,T](zio: => ZIO[R & Setup,E,T]): ZIO[R,E,T] = for {
    setup <- Setup.make
    res <- zio.provideSome[R](ZLayer.succeed(setup))
    _ <- setup.start
  } yield res
}
