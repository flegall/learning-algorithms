package algorithms.sorting

import org.scalatest.{FunSpec, Matchers}

class SortingSpec extends FunSpec with Matchers {
  describe("Sorting algorithms") {
    describe("Selection sort") {
      it("should sort an array") {
        val array = anArrayToSort()

        SelectionSort.sort(array)

        array shouldBe sorted
      }
    }
  }
  def anArrayToSort() =
    Array("z", "a", "n", "a", "r", "r", "a", "y", "t", "o", "s", "o", "r", "t")
}
