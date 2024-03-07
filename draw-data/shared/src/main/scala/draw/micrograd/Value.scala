package draw.micrograd

class Value(initial: Double, gradients: Seq[(Value, Double)]) { src =>
  private[Value] var data: Double = initial
  private[Value] var grad: Double = 0

  override def toString = s"Value($data, $grad)"

  private[Value] def backwardStep(): Unit = {
    for ((child, gradient) <- gradients) {
      child.grad += gradient * grad
    }
  }

  private[Value] def children: Iterable[Value] = gradients.map(_._1)

  def gradient = grad
  def value = data

  def +(other: Value): Value = {
    new Value(data + other.data, Seq(
      src -> 1.0,
      other -> 1.0
    ))
  }
  def +(other: Double): Value = this + Value(other)

  def -(other:Value): Value = this + (other * -1)
  def -(other: Double): Value = this - Value(other)

  def *(other: Value): Value = {
    new Value(data * other.data, Seq(
      src -> other.data,
      other -> src.data
    ))
  }
  def *(other: Double): Value = this * Value(other)

  def /(other: Value) = this * other.pow(-1)
  def /(other: Double) = this * Math.pow(other, -1)

  def tanh: Value = {
    val res = (Math.exp(2 * data) - 1) / (Math.exp(2 * data) + 1)
    new Value(res, Seq(
      src -> (1 - Math.pow(res, 2))
    ))
  }

  def exp: Value = {
    val res = Math.exp(data)
    new Value(res, Seq(
      src -> res
    ))
  }

  def pow(other: Double): Value = {
    new Value(Math.pow(data, other), Seq(
      src -> other * Math.pow(data, other - 1)
    ))
  }

  def backward(): Unit = {
    val topo = Vector.newBuilder[Value]
    var visited = Set.empty[Value]

    def build(v: Value): Unit = {
      if (!visited.contains(v)) {
        // Reset gradient here, because why not
        v.grad = 0
        visited += v
        v.children.foreach(build)
        topo += v
      }
    }

    build(this)

    grad = 1.0
    topo.result().reverse.foreach(_.backwardStep())
  }
}

object Value {
  def apply(constant: Double): Value = new Value(constant, Seq.empty)

  implicit class DoubleOps(d: Double) extends AnyVal {
    def +(v: Value): Value = Value(d) + v
    def -(v: Value): Value = Value(d) - v
    def *(v: Value): Value = Value(d) * v
  }

  implicit class IntOps(i: Int) extends AnyVal {
    def +(v: Value): Value = Value(i.toDouble) + v
    def -(v: Value): Value = Value(i.toDouble) - v
    def *(v: Value): Value = Value(i.toDouble) * v
  }
}
