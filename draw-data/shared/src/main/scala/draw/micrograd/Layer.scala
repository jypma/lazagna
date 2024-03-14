package draw.micrograd

import zio.Chunk
import zio.ZIO

/** A fully-connected layer, represented by a chunk of neurons. All neurons must have the same number of
  * weights. */
case class Layer(neurons: Chunk[Neuron]) {
  /** Applies the given inputs to this layer, by presenting them to each neuron. The number of inputs must match
    * the number of weights of this layer's neurons. */
  def apply(inputs: Chunk[Value]): Chunk[Value] = neurons.map(_(inputs))

  def parameters: Chunk[Value] = neurons.flatMap(_.parameters)

  override def toString = s"\n  Layer(${neurons.mkString(", ")})"
}

object Layer {
  def make(inputCount: Int, outputCount: Int) = for {
    neurons <- ZIO.collectAll(new Array[Byte](outputCount).map(_ => Neuron.make(inputCount)))
  } yield Layer(Chunk.fromArray(neurons))
}
