package draw.micrograd

import zio.test.ZIOSpecDefault
import zio.test.Assertion._
import zio.test._

object ValueTest extends ZIOSpecDefault {
  override def spec = suite("ValueTest")(
    test("add itself") {
      val a = Value(3)
      val r = a + a
      r.backward()
      assertTrue(a.gradient == 2)
    },

    test("add and multiply") {
      val a = Value(-2)
      val b = Value(3)
      val d = a * b
      val e = a + b
      val f = d * e
      f.backward()
      assertTrue(a.gradient == -3 && b.gradient == -8)
    }
  )

}
