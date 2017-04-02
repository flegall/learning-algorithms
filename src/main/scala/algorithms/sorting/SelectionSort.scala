package algorithms.sorting

import scala.math.Ordering

object SelectionSort {
  def apply[T: Ordering](t: Array[T]): Unit = {
    val ord: Ordering[T] = implicitly[Ordering[T]]
    val N = t.length

    for (i <- 0 until N) {
      var min = i

      for (j <- i + 1 until N) {
        if (ord.lt(t(j), t(min))) {
          min = j
        }
      }

      swap(t, min, i)
    }
  }
}
