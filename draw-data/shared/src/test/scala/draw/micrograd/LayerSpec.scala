package draw.micrograd

import zio.test.ZIOSpecDefault
import zio.test._
import zio.Chunk

object LayerSpec extends ZIOSpecDefault {
  override def spec = suite("LayerSpec")(
    test("has right size") {
      for {
        layer <- Layer.make(inputCount = 2, outputCount = 3)
      } yield {
        val res = layer(Chunk(2.0, 3.0))
        assertTrue(res.size == 3, res.forall(v => v.value >= -1.0 && v.value <= 1.0))
      }
    }
  )
}
