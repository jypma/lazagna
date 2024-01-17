package draw

import zio.{ExitCode, Scope, ZIO, ZIOAppDefault}

import org.scalajs.dom

object Main extends ZIOAppDefault {

  val root = dom.document.querySelector("#app")

  def logCommandFailure(f: DrawCommand.Failed) = ZIO.succeed {
    dom.console.log(f.message)
  }

  val main: ZIO[DrawingRenderer & Scope, Nothing, Unit] = for {
    renderer <- ZIO.service[DrawingRenderer]
    _ <- renderer.render.mount(root)
  } yield ()

  override def run = ZIO.scoped {
    (for {
      _ <- main
      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success)
      .catchAllCause { cause =>
      dom.console.log(cause.prettyPrint)
      ZIO.succeed(ExitCode.failure)
    }
  }.provide(Drawing.live, DrawingRenderer.live)
}
