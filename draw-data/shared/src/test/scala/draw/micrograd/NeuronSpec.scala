package draw.micrograd

import zio.test.ZIOSpecDefault
import zio.test._
import zio.Chunk

object NeuronSpec extends ZIOSpecDefault {
  override def spec = suite("NeuronSpec")(
    test("is in range") {
      for {
        n <- Neuron.make(2)
      } yield {
        val res = n(Chunk(2.0, 3.0))
        assertTrue(res.value >= -1.0, res.value <= 1.0)
      }
    }
  )
}
