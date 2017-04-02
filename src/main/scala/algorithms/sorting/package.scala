package algorithms

import scala.math.Ordering

package object sorting {
  def swap[T](t: Array[T], a: Int, b: Int): Unit = {
    val q = t(a)
    t(a) = t(b)
    t(b) = q
  }

  def move_element_at_index_to_sorted_position_on_the_left[T](
      t: Array[T],
      i: Int,
      ordering: Ordering[T],
      step: Int = 1): Unit = {
    val v = t(i)
    var j = i
    while (j >= step && ordering.gt(t(j - step), v)) {
      t(j) = t(j - step)
      j -= step
    }
    t(j) = v
  }
}
