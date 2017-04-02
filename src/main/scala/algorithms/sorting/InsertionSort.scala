package algorithms.sorting

import scala.math.Ordering

object InsertionSort {
  def apply[T: Ordering](t: Array[T]): Unit = {
    val ordering = implicitly[Ordering[T]]

    for (i <- 1 until t.length) {
      move_element_at_index_to_sorted_position_on_the_left(t, i, ordering)
    }
  }
}
