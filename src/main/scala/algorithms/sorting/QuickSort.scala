package algorithms.sorting

import scala.collection.mutable
import scala.math.Ordering
import scala.reflect.ClassTag
import scala.util.control.Breaks._

object QuickSort {

  def apply[T: Ordering](t: Array[T]): Unit = {
    val ordering = implicitly[Ordering[T]]
    val stack = mutable.Stack[Int]()

    var l = 0
    var r = t.length - 1

    breakable {
      while (true) {
        while (r > l) {
          val i = partition(t, l, r, ordering)
          if (i - l > r - i) {
            stack.push(l, i - 1)
            l = i + 1
          } else {
            stack.push(i + 1, r)
            r = i - 1
          }
        }

        if (stack.isEmpty) {
          break
        } else {
          r = stack.pop()
          l = stack.pop()
        }
      }
    }
  }

  def recursive[T: Ordering](t: Array[T])(l: Int = 0,
                                          r: Int = t.length - 1): Unit = {
    val ordering = implicitly[Ordering[T]]

    if (r > l) {
      val i = partition(t, l, r, ordering)

      QuickSort.recursive(t)(l, i - 1)
      QuickSort.recursive(t)(i + 1, r)
    }
  }

  private def partition[T: Ordering](t: Array[T],
                                     l: Int,
                                     r: Int,
                                     ordering: Ordering[T]): Int = {
    val v = t(r)
    var i = l - 1
    var j = r

    breakable {
      while (true) {
        do { i += 1 } while (ordering.lt(t(i), v))
        do { j -= 1 } while (j >= 0 && ordering.gt(t(j), v))
        if (i >= j) { break() }
        swap(t, i, j)
      }
    }
    swap(t, i, r)

    i
  }

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
