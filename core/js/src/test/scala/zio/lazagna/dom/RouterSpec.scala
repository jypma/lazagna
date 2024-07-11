package zio.lazagna.dom

import zio.test._
import zio.ZIO
import zio.UIO

import org.scalajs.dom

object RouterSpec extends ZIOSpecDefault {
  import Router._

  def runFixture(hash: String): UIO[String] = {
    dom.document.location.hash = hash;
    for {
      res <- zio.Ref.make("")
      r = router(
        route(/("foo") / "bar") { _ => res.set("foobar") },
        route(/("foo")) { _ => res.set("foo") },
        route(/("idparam") && param("id").asUUID) { id => res.set(id.toString) },
        route(all) { _ => res.set("fallback") }
      )
      _ <- ZIO.scoped(r.mount(dom.document.documentElement))
      out <- res.get
    } yield out
  }

  override def spec = suite("Router")(
    test("should fallback on no hash") {
      runFixture("").map(r => assertTrue(r == "fallback"))
    },

    test("should fallback on empty hash") {
      runFixture("#").map(r => assertTrue(r == "fallback"))
    },

    test("should fallback when no routes match") {
      runFixture("#elephant").map(r => assertTrue(r == "fallback"))
    },

    test("should match by path") {
      runFixture("#foo").map(r => assertTrue(r == "foo"))
    },

    test("should match by subpath") {
      runFixture("#foo/bar").map(r => assertTrue(r == "foobar"))
    },

    test("should match by subpath, ignoring leading slash") {
      runFixture("#/foo/bar").map(r => assertTrue(r == "foobar"))
    },

    test("should ignore a non-UUID param") {
      runFixture("#/idparam?id=foobar").map(r => assertTrue(r == "fallback"))
    },

    test("should match a UUID param") {
      runFixture("#/idparam?id=587ed367-7357-4592-8c23-a1aeb7642701").map(r => assertTrue(r == "587ed367-7357-4592-8c23-a1aeb7642701"))
    }
  ) @@ TestAspect.sequential
}
