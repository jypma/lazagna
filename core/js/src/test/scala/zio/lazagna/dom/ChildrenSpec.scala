package zio.lazagna.dom

import zio.test._
import zio.ZIO
import zio.lazagna.Consumeable._

import org.scalajs.dom

import Element.tags._
import Element._
import zio.Queue
import Fixture._

object ChildrenSpec extends ZIOSpecDefault {
  /* Wait for two dummy messages, which will ensure that any earlier messages are consumed (since the queue has capacity 1) */
  def awaitEmpty(queue: Queue[Children.ChildOp]) = {
    queue.offer(Children.Delete(div())).repeatN(2)
  }

  override def spec = suite("Children")(
    test("should add a child") {
      val child = span(textContent := "child")
      for {
        owner <- makeDiv
        queue <- Queue.bounded[Children.ChildOp](1)
        _ <- div(
          children <~~ queue
        ).mount(owner)
        _ <- queue.offer(Children.Append(child))
        _ <- awaitEmpty(queue)
      } yield {
        assertTrue(owner.innerHTML == "<div><span>child</span></div>")
      }
    },

    test("should add a child only once") {
      val child = span(textContent := "child")
      for {
        owner <- makeDiv
        queue <- Queue.bounded[Children.ChildOp](1)
        _ <- div(
          children <~~ queue
        ).mount(owner)
        _ <- queue.offer(Children.Append(child))
        _ <- queue.offer(Children.Append(child))
        _ <- awaitEmpty(queue)
      } yield {
        assertTrue(owner.innerHTML == "<div><span>child</span></div>")
      }
    },

    test("should delete a child") {
      val child = span(textContent := "child")
      for {
        owner <- makeDiv
        queue <- Queue.bounded[Children.ChildOp](1)
        _ <- div(
          children <~~ queue
        ).mount(owner)
        _ <- queue.offer(Children.Append(child))
        _ <- queue.offer(Children.Delete(child))
        _ <- awaitEmpty(queue)
      } yield {
        assertTrue(owner.innerHTML == "<div></div>")
      }
    },
  )
}
