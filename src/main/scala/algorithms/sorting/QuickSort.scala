package algorithms.sorting

import scala.math.Ordering
import scala.reflect.ClassTag

object QuickSort {
  def naiveRecursive[T: Ordering](t: Array[T])(
      implicit classTag: ClassTag[T]): Array[T] = {
    val ordering = implicitly[Ordering[T]]

    t.headOption match {
      case None => Array.empty
      case Some(first) =>
        val tail = t.tail

        val below = tail.filter(value => ordering.lt(value, first))
        val after = tail.filter(value => ordering.gt(value, first))

        naiveRecursive(below) ++ Array(first) ++ naiveRecursive(after)
    }
  }
}
