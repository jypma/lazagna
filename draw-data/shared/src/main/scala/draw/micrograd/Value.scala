package draw.micrograd

class Value(initial: Double) { src =>
  private[Value] var data: Double = initial
  private[Value] var grad: Double = 0

  override def toString = s"Value($data, $grad)"

  def gradient = grad
  def value = data

  def +(other: Value): Value = {
    new Value(data + other.data) {
      // TODO: Refactor to just take the children, and allow it to return by what delta to add gradient there.
      override def backwardStep() = {
        src.grad += 1.0 * grad
        other.grad += 1.0 * grad
      }
      override val children = Seq(src, other)
    }
  }
  def +(other: Double): Value = this + Value(other)

  def -(other: Value): Value = {
    new Value(data - other.data) {
      override def backwardStep() = {
        src.grad += 1.0 * grad
        other.grad += 1.0 * grad
      }
      override val children = Seq(src, other)
    }
  }
  def -(other: Double): Value = this - Value(other)

  def *(other: Value): Value = {
    new Value(data * other.data) {
      override def backwardStep() = {
        src.grad += other.data * grad
        other.grad += src.data * grad
      }
      override val children = Seq(src, other)
    }
  }
  def *(other: Double): Value = this * Value(other)

  def tanh: Value = {
    val t = (Math.exp(2 * data) - 1) / (Math.exp(2 * data) + 1)
    new Value(t) {
      override def backwardStep() = {
        src.grad = (1 - Math.pow(t, 2)) * grad
      }
      override val children = Seq(src)
    }
  }

  private[Value] def backwardStep(): Unit = {}
  private[Value] def children: Iterable[Value] = Nil

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
