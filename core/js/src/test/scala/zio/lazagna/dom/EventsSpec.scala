package zio.lazagna.dom

import zio.test.ZIOSpecDefault
import zio.ZIO
import Element.tags._
import Events._
import zio.lazagna.dom.Modifier.MountPoint
import zio.lazagna.Setup
import zio.ZLayer
import org.scalajs.dom
import org.scalajs.dom.MouseEvent
import org.scalajs.dom.MouseEventInit
import zio.test._
import zio.Exit
import zio.lazagna.dom.Element.thisElementAs

object EventsSpec extends ZIOSpecDefault {
  import Fixture._

  def click(target: dom.Element): Unit = {
    val event = new MouseEvent("click", new MouseEventInit {
      bubbles = true
      view = dom.window
    })
    target.dispatchEvent(event)
  }

  override def spec = suite("Events")(
    test("should invoke listener while mounted") {
      var clicks = 0
      var toClick: dom.Element = null
      for {
        owner <- makeDiv
        _ <- div(
          thisElementAs { d =>
            toClick = d;
            ZIO.unit
          },
          onClick(_.map { _ => clicks += 1 })
        ).mount(owner)
        _ = click(toClick)
        _ <- await(clicks > 0)
      } yield {
        assertTrue(clicks == 1)
      }
    },

    test("should not invoke listener after scope is closed") {
      var clicks = 0
      var toClick: dom.Element = null
      for {
        owner <- makeDiv
        parent <- ZIO.scope
        scope <- parent.fork
        _ <- div(
          thisElementAs { d =>
            toClick = d;
            ZIO.unit
          },
          onClick(_.map { _ => clicks += 1 }).run.provideSome[MountPoint & Setup](ZLayer.succeed(scope))
        ).mount(owner)
        _ = click(toClick)
        _ <- scope.close(Exit.unit)
        _ = click(toClick)
        _ <- await(clicks > 0)
      } yield {
        assertTrue(clicks == 1)
      }
    }

  )
}
