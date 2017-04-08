package algorithms.sorting

import java.util.UUID.randomUUID

import org.scalatest.{FunSpec, Matchers}

class SortingSpec extends FunSpec with Matchers {
  describe("Sorting algorithms") {

    describe("Selection sort") {
      it("should sort a simple array") {
        val array = anArrayToSort()

        SelectionSort(array)

        array shouldBe sorted
      }

      it("should sort a huge array") {
        val array = aHugeArray()

        SelectionSort(array)

        array shouldBe sorted
      }
    }

    describe("Insertion sort") {
      it("should sort a simple array") {
        val array = anArrayToSort()

        InsertionSort(array)

        array shouldBe sorted
      }

      it("should sort a huge array") {
        val array = aHugeArray()

        InsertionSort(array)

        array shouldBe sorted
      }
    }

    describe("Shell sort") {
      it("should sort a simple array") {
        val array = anArrayToSort()

        ShellSort(array)

        array shouldBe sorted
      }

      it("should sort a huge array") {
        val array = aHugeArray()

        ShellSort(array)

        array shouldBe sorted
      }
    }

    describe("Quick sort") {
      describe("naive recursive implementation") {
        it("should sort a simple array") {
          var array = anArrayToSort()

          array = QuickSort.naiveRecursive(array)

          array shouldBe sorted
        }

        it("should sort a huge array") {
          var array = aHugeArray()

          array = QuickSort.naiveRecursive(array)

          array shouldBe sorted
        }
      }

      describe("recursive implementation") {
        it("should sort a simple array") {
          val array = anArrayToSort()

          QuickSort.recursive(array)()

          array shouldBe sorted
        }

        it("should sort a huge array") {
          val array = aHugeArray()

          QuickSort.recursive(array)()

          array shouldBe sorted
        }
      }
    }
  }

  def anArrayToSort(): Array[String] =
    Array("z", "a", "n", "a", "r", "r", "a", "y", "t", "o", "s", "o", "r", "t")

  def aHugeArray(): Array[String] =
    (0 until 2000)
      .map(_ => randomUUID)
      .map(_.toString)
      .toArray[String]
}
