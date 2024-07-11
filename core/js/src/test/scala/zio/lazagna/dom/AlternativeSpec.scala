package zio.lazagna.dom

import zio.test._
import zio.ZIO
import zio.lazagna.Consumeable._

import org.scalajs.dom

import Element.tags._
import Element._
import zio.Queue
import Fixture._

object AlternativeSpec extends ZIOSpecDefault {

  override def spec = suite("Alternatives")(
    suite("mountOne")(
      test("should initialize empty") {
        for {
          owner <- makeDiv("owner")
          queue <- Queue.bounded[String](1)
          _ <- div(
            Alternative.mountOne(queue) { s => span(textContent := s) }
          ).mount(owner)
        } yield {
          assertTrue(owner.innerHTML == "<div></div>")
        }
      },

      test("should add an initial item") {
        for {
          owner <- makeDiv("owner")
          queue <- Queue.bounded[String](1)
          _ <- div(
            Alternative.mountOne(queue) { s => span(textContent := s) }
          ).mount(owner)
          _ <- queue.offer("one")
          _ <- await(owner.innerHTML.contains("one"))
        } yield {
          assertTrue(owner.innerHTML == "<div><span>one</span></div>")
        }
      },

      test("should close previous scope when switching items") {
        for {
          owner <- makeDiv("owner")
          queue <- Queue.bounded[String](1)
          _ <- div(
            Alternative.mountOne(queue) { s => span(textContent := s) }
          ).mount(owner)
          _ <- queue.offer("one")
          _ <- queue.offer("two")
          _ <- await(owner.innerHTML.contains("two"))
        } yield {
          assertTrue(owner.innerHTML == "<div><span>two</span></div>")
        }
      },
    )
  )
}
