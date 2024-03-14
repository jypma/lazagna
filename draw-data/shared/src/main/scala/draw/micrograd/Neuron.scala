package draw.micrograd

import zio.Chunk
import zio.Random
import zio.ZIO

/** A neuron multiplies its inputs with its weights, and adds a bias.*/
case class Neuron(bias: Value, weights: Chunk[Value]) {
  /** Applies the given inputs to this neuron. The number of inputs must match the number of weights in this
    * neuron. */
  def apply(inputs: Chunk[Value]): Value = {
    val act = inputs.zipWith(weights)(_ * _).foldLeft(bias)(_ + _)
    act.tanh
  }

  def parameters: Chunk[Value] = weights :+ bias

  override def toString = s"\n    Neuron($bias, [${weights.mkString(", ")}]))"
}

object Neuron {
  def make(inputCount: Int) = for {
    bias <- Random.nextDoubleBetween(-1, 1)
    weights <- ZIO.collectAll(Chunk.fromArray(new Array[Byte](inputCount)).map(_ => Random.nextDoubleBetween(-1, 1)))
  } yield Neuron(Value(bias), weights.map(Value(_)))
}
