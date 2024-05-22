package draw.client

import zio.lazagna.dom.Attribute._
import zio.lazagna.dom.Element.tags._
import zio.lazagna.dom.Element.{textContent, _}
import zio.lazagna.dom.http.Request.{AsString, GET}
import zio.{Scope, ZIO, ZLayer}

import org.scalajs.dom

trait LoginHandler {
  /** Renders a login view as the current page (with links to OAuth providers) */
  def render(links: Seq[(String, String)]): ZIO[Scope, Nothing, dom.HTMLElement]
  /** Handles a callback with a code (and reloads the page if successful) */
  def handleCode(code: String, state: String): ZIO[Scope, Nothing, Unit]
}

object LoginHandler {
  val live = ZLayer.fromZIO(make)

  def make = for {
    config <- ZIO.service[DrawingClient.Config]
  } yield new LoginHandler {
    def render(links: Seq[(String, String)]) = {
      div(
        cls := "links",
        links.map { case (name, link) =>
          div(
            cls := "link",
            a(
              href := s"${link}&state=/",
              textContent := name
            )
          )
        }
      ).mount(dom.document.querySelector("#app"))
    }

    def handleCode(code: String, state: String) = {
      for {
        user <- GET(AsString, s"${config.baseUrl}/user/activate?code=${code}&state=${state}").catchAll { err =>
          dom.console.log(err)
          div(
            textContent := "Login unsuccessful."
          ).mount(dom.document.querySelector("#app"))
        }
      } yield {
        println(user)
        dom.window.location.replace("/")
      }
    }
  }
}
