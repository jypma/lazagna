package draw.micrograd

import zio.Chunk
import zio.ZIO

/** An MLP is a series of fully-connected layers. */
case class MultiLayerPerceptron(layers: Chunk[Layer]) {
  /** Applies the inputs to the first layer, and then through each layer in series. The number of inputs must
    * match the input size of the first layer. */
  def apply(inputs: Chunk[Value]): Chunk[Value] = layers.foldLeft(inputs)((input, layer) => layer(input))

  def parameters: Chunk[Value] = layers.flatMap(_.parameters)
}

object MultiLayerPerceptron {
  /** Creates an MLP with the number of inputs, and the given number of outputs for respective layers. */
  def make(inputCount: Int, outputCounts: Chunk[Int]) = {
    val sizes = inputCount +: outputCounts
    for {
      layers <- ZIO.collectAll((0 until outputCounts.size).map(i => Layer.make(sizes(i), sizes(i+1))))
    } yield MultiLayerPerceptron(Chunk.fromIterable(layers))
  }
}
