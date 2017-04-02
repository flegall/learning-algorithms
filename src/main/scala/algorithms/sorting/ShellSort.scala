package algorithms.sorting

import scala.math.Ordering

object ShellSort {
  def apply[T: Ordering](t: Array[T]): Unit = {
    val ordering = implicitly[Ordering[T]]

    // computing h value
    var h = 1
    while (h <= t.length / 9) {
      h = 3 * h + 1
    }

    while (h > 0) {
      for (i <- h until t.length) {
        // Sorting each a h-zone
        move_element_at_index_to_sorted_position_on_the_left(t,
                                                             i,
                                                             ordering,
                                                             step = h)
      }
      h /= 3
    }
  }
}
