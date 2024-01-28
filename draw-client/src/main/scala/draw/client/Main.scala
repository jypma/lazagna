package draw.client

import zio.{ExitCode, Scope, ZIO, ZIOAppDefault}

import org.scalajs.dom
import zio.ZLayer

object Main extends ZIOAppDefault {

  def main = for {
    renderer <- ZIO.service[DrawingRenderer]
    drawing <- ZIO.service[Drawing]
    tools <- ZIO.service[DrawingTools]
    _ <- renderer.render.mount(dom.document.querySelector("#app"))
    _ <- tools.renderToolbox.mount(dom.document.querySelector("#toolboxApp"))
  } yield ()

  override def run = ZIO.scoped {
    (for {
      client <- ZIO.service[DrawingClient]
      drawing <- client.login("jan", "jan", "test")
      _ <- main.provideSome[Scope](ZLayer.succeed(drawing), DrawingTools.live, DrawingRenderer.live)
      _ = dom.console.log("Main is ready.")
      _ <- ZIO.never // We don't want to exit main, since our background fibers do the real work.
    } yield ExitCode.success)
      .catchAllCause { cause =>
      dom.console.log(cause.prettyPrint)
      ZIO.succeed(ExitCode.failure)
    }
  }.provide(DrawingClient.live, DrawingClient.configTest)
}
