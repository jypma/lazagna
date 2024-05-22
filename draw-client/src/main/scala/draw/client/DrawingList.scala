package draw.client

import zio.lazagna.Setup
import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.{textContent, _}
import zio.{Scope, ZIO, ZLayer}

import draw.client.DrawingClient.ClientError
import org.scalajs.dom

trait DrawingList {
  def render: ZIO[Scope & Setup, ClientError, Unit]
}

object DrawingList {
  def live = ZLayer.fromZIO(for {
    client <- ZIO.service[DrawingClient]
  } yield new DrawingList {
    def view(list: Seq[DrawingClient.DrawingRef]) = div(
      cls := "dialog",
      div(
        cls := "drawing-list",
        div(
          cls := "title",
          textContent := "Please select a drawing"
        ),
        list.map { drawing =>
          div(
            cls := "drawing",
            div(
              cls := "name",
              a(
                href := s"#?id=${drawing.id}",
                textContent := drawing.name
              )
            )
          )
        }
      )
    )

    override def render = {
      for {
        list <- client.list
        _ <- view(list).mount(dom.document.querySelector("#app"))
      } yield ()
    }
  })
}
