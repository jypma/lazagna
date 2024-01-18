package zio.lazagna

import zio.UIO
import zio.ZIO
import zio.Ref
import zio.ZLayer

// TODO: Maybe this should just be a seq of Promise[Unit] to wait for, rather than full-on ZIO units.
trait Setup {
  def addStartAction(action: UIO[Unit]): UIO[Unit]
}

object Setup {
  sealed trait Startable extends Setup {
    def start: UIO[Unit]
  }

  def make: UIO[Setup.Startable] = {
    for {
      actions <- Ref.make[Seq[UIO[Unit]]](Seq.empty)
    } yield new Setup.Startable {
      override def addStartAction(action: UIO[Unit]) = actions.update(_ :+ action)
      override def start = actions.get.flatMap(ZIO.collectAll).unit
    }
  }

  def start[R,E,T](zio: => ZIO[R & Setup,E,T]): ZIO[R,E,T] = for {
    setup <- Setup.make
    res <- zio.provideSome[R](ZLayer.succeed(setup))
    _ <- setup.start
  } yield res
}
