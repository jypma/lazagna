package draw.client

import zio.{ExitCode, Scope, ZIO, ZIOAppDefault}

import org.scalajs.dom

object Main extends ZIOAppDefault {

  val root = dom.document.querySelector("#app")

  def main(drawing: Drawing): ZIO[DrawingRenderer & Scope, Nothing, Unit] = for {
    renderer <- ZIO.service[DrawingRenderer]
    _ <- renderer.render(drawing).mount(root)
  } yield ()

  override def run = ZIO.scoped {
    (for {
      client <- ZIO.service[DrawingClient]
      drawing <- client.login("jan", "jan", "test")
      _ <- main(drawing)
      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success)
      .catchAllCause { cause =>
      dom.console.log(cause.prettyPrint)
      ZIO.succeed(ExitCode.failure)
    }
  }.provide(DrawingRenderer.live, DrawingClient.live, DrawingClient.configTest)
}
