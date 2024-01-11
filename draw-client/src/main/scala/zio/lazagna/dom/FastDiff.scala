package zio.lazagna.dom

object FastDiff {
  sealed trait Op[T] {
    def run(in:Seq[T]): Seq[T]
  }
  /** Inserts the given element at the given position (or move it if it's already in the destination) */
  case class Insert[T](elem: T, position: Int) extends Op[T] {
    override def run(in: Seq[T]) = {
      val index = in.indexOf(elem)
      if (index == -1) {
        in.patch(position, Seq(elem), 0)
      } else if (index < position) {
        in.patch(position, Seq(elem), 0).patch(index, Seq.empty, 1)
      } else if (index > position) {
        in.patch(index, Seq.empty, 1).patch(position, Seq(elem), 0)
      } else { // same position
        in.updated(index, elem)
      }
    }
  }
  case class Delete[T](position: Int) extends Op[T] {
    override def run(in: Seq[T]) = in.patch(position, Seq.empty, 1)
  }

  def diff[T <: AnyRef](current: Seq[T], wanted: Seq[T]): Seq[Op[T]] = {
    val res = Seq.newBuilder[Op[T]]

    var count = current.length // Actual number of live items while we're looping
    var wantedIndex = 0
    var currentIndex = 0
    for (t <- wanted) {
      println(s"Now at wanted=${wantedIndex} current=${currentIndex} with ${count}, checking ${t}")
      if (wantedIndex >= count) {
        // We've already re-used all the [src] items we can, so we can just add this one.
        res += Insert(t, wantedIndex)
        count += 1
      } else {
        if (current(currentIndex) eq t) {
          // Item already matches
          currentIndex += 1
        } else {
          if (!current.contains(t)) {
            // TODO: Add a Set for the above check.
            // The item is new, so we can just insert it here.
            res += Insert(t, wantedIndex)
            count += 1
          } else {
            // We already have the item, but at a different position.

            // Since we've encountered a [current] item here that isn't [t]. let's check for deletions and apply those first.
            while (!(current(currentIndex) eq t) && !wanted.contains(current(currentIndex))) {
              res += Delete(currentIndex)
              count -= 1
              currentIndex += 1
            }

            if (current(currentIndex) eq t) {
              // After deleting, we now have found the item at the right position.
              currentIndex += 1
            } else {
              // Move the item to the right position (no change in count)
              res += Insert(t, wantedIndex)
            }
          }
        }
      }

      wantedIndex += 1
    }

    res.result()
  }
}
