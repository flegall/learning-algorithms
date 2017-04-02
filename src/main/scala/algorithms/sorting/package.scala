package algorithms

package object sorting {
  def swap[T](t: Array[T], a: Int, b: Int): Unit = {
    val q = t(a)
    t(a) = t(b)
    t(b) = q
  }
}
