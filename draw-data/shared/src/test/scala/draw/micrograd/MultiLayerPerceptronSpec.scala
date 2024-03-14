package draw.micrograd

import zio.test.ZIOSpecDefault
import zio.test._
import zio.Chunk

object MultiLayerPerceptronSpec extends ZIOSpecDefault {
  val trainingExamples: Chunk[Chunk[Value]] = Chunk(
    Chunk(2.0, 3.0, -1.0),
    Chunk(3.0, -1.0, 0.5),
    Chunk(0.5, 1.0, 1.0),
    Chunk(1.0, 1.0, -1.0)
  )
  // Expected output for each training example
  val trainingTargets: Chunk[Value] = Chunk(
    1.0, -1.0, -1.0, 1.0
  )

  override def spec = suite("MLPSpec")(
    test("has right size") {
      for {
        mlp <- MultiLayerPerceptron.make(inputCount = 3, outputCounts = Chunk(4, 4, 1))
      } yield {
        val res = mlp(Chunk(2.0, 3.0, -1.0))
        assertTrue(
          res.size == 1,
          res.forall(v => v.value >= -1.0 && v.value <= 1.0),
          mlp.parameters.size == 41
        )
      }
    },

    test("can learn a simple network") {
      for {
        mlp <- MultiLayerPerceptron.make(inputCount = 3, outputCounts = Chunk(4, 4, 1))
      } yield {
        def performStep(stepSize: Double): Value = {
          val predictions: Chunk[Value] = trainingExamples.map(mlp(_).head) // head because output layer only is size one
                                                                            // Mean square error loss
          val loss: Value = predictions.zipWith(trainingTargets)((a,b) => (a - b).pow(2)).foldLeft(Value.zero)(_ + _)
          loss.backward()

          mlp.parameters.foreach(_.adjust(stepSize))
          loss
        }

        for (i <- 1 to 50) {
          performStep(0.05)
        }

        val finalPredictions = trainingExamples.map(mlp(_).head)
        assertTrue(finalPredictions.zip(trainingTargets).forall((a,b) => Math.abs(a.value - b.value) < 0.1))
      }
    }
  )
}
