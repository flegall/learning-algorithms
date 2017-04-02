package algorithms.sorting

import scala.math.Ordering

object InsertionSort {
  def apply[T: Ordering](t: Array[T]): Unit = {
    val ordering = implicitly[Ordering[T]]

    for (i <- 1 until t.length) {
      move_element_at_index_to_sorted_position_on_the_left(t, i, ordering)
    }
  }

  def move_element_at_index_to_sorted_position_on_the_left[T](
      t: Array[T],
      i: Int,
      ordering: Ordering[T]): Unit = {
    val v = t(i)
    var j = i
    while (j >= 1 && ordering.gt(t(j - 1), v)) {
      t(j) = t(j - 1)
      j -= 1
    }
    t(j) = v
  }
}
