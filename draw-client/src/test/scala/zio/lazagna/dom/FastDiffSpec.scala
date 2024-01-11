package zio.lazagna.dom

import zio.test._

object FastDiffSpec extends ZIOSpecDefault {
  import FastDiff._

  case class Elem(content: String)

  def runOps(from: Seq[Elem], ops: Seq[Op[Elem]]) = ops.foldLeft(from)((res, op) => op.run(res))

  def spec =
    suite("FastDiff.diff")(
      test("should yield no operations on empty collections") {
        assertTrue(diff[Elem](Seq.empty, Seq.empty).isEmpty)
      },

      test("should yield no operations on equal collections") {
        val seq = Seq(Elem("1"), Elem("2"), Elem("3"))
        assertTrue(runOps(seq, diff[Elem](seq, seq)) == seq)
       },

      test("should yield a move from start to end") {
        val from = Seq(Elem("1"), Elem("2"), Elem("3"))
        val to = Seq(Elem("2"), Elem("3"), Elem("1"))
        assertTrue(runOps(from, diff[Elem](from, to)) == to)
      },

      test("should yield a move from end to start") {
        val from = Seq(Elem("1"), Elem("2"), Elem("3"))
        val to = Seq(Elem("3"), Elem("1"), Elem("2"))
        assertTrue(runOps(from, diff[Elem](from, to)) == to)
      }

    )
}
