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
      assertTrue(a.gradient == -3, b.gradient == -8)
    },

    test("power") {
      val a = Value(3)
      val r = a.pow(2)
      r.backward()
      assertTrue(r.value == 9, a.gradient == 6)
    },

    test("division") {
      val a = Value(-2)
      val b = Value(3)
      val r = a / b
      r.backward()
      assertTrue(r.value == -2.0/3.0, a.gradient == 1.0/3.0, b.gradient == 2.0/9.0)
    },

    test("tanh neuron") {
      val x1 = Value(2)
      val x2 = Value(0)
      val w1 = Value(-3)
      val w2 = Value(1)
      val b = Value(6.8813735870195432)
      val n = (x1 * w1) + (x2 * w2) + b
      val o = n.tanh
      o.backward()
      assertTrue(
        x1.gradient ==~ -1.5,
        w1.gradient ==~ 1.0,
        x2.gradient ==~ 0.5,
        w2.gradient ==~ 0.0
      )
    },

    test("exp neuron") {
      // Same test above, but with tanh expressed as exp
      val x1 = Value(2)
      val x2 = Value(0)
      val w1 = Value(-3)
      val w2 = Value(1)
      val b = Value(6.8813735870195432)
      val n = (x1 * w1) + (x2 * w2) + b
      val n2 = (n * 2)
      val e = n2.exp
      val em = (e - 1)
      val ep = (e + 1)
      val o = em / ep
      o.backward()
      assertTrue(
        x1.gradient ==~ -1.5,
        w1.gradient ==~ 1.0,
        x2.gradient ==~ 0.5,
        w2.gradient ==~ 0.0
      )
    }
  )

  implicit class DoubleOps (a: Double) {
    inline def ==~(b: Double): Boolean = Math.abs(b - a) < 0.00000000001
  }
}
