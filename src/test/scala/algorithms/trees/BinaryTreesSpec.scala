package algorithms.trees

import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable

class BinaryTreesSpec extends FunSpec with Matchers {
  describe("Binary trees") {

    describe("depth-first search") {

      describe("in pre-order (iterative)") {

        it("should visit nodes in pre-order") {
          val buffer = mutable.ListBuffer.empty[String]
          val tree = anExampleTree()

          tree.traversePreOrderIterative((value) => buffer += value)

          buffer should contain inOrderOnly ("F", "B", "A", "D", "C", "E", "G", "I", "H")
        }
      }

      describe("in pre-order (recursive)") {

        it("should visit nodes in pre-order") {
          val buffer = mutable.ListBuffer.empty[String]
          val tree = anExampleTree()

          tree.traversePreOrderRecursive((value) => buffer += value)

          buffer should contain inOrderOnly ("F", "B", "A", "D", "C", "E", "G", "I", "H")
        }
      }

      describe("in order (recursive)") {

        it("should visit nodes in order") {
          val buffer = mutable.ListBuffer.empty[String]
          val tree = anExampleTree()

          tree.traverseInOrderRecursive((value) => buffer += value)

          buffer shouldBe sorted
        }
      }


      describe("in order (iterative)") {

        it("should visit nodes in order") {
          val buffer = mutable.ListBuffer.empty[String]
          val tree = anExampleTree()

          tree.traverseInOrderIterative((value) => buffer += value)

          buffer shouldBe sorted
        }
      }

      describe("in post-order (recursive)") {

        it("should visit nodes in post-order") {
          val buffer = mutable.ListBuffer.empty[String]
          val tree = anExampleTree()

          tree.traverseInPostOrderRecursive((value) => buffer += value)

          buffer should contain inOrderOnly ("A", "C", "E", "D", "B", "H", "I", "G", "F")
        }
      }

      describe("in post-order (iterative)") {

        it("should visit nodes in post-order") {
          val buffer = mutable.ListBuffer.empty[String]
          val tree = anExampleTree()

          tree.traverseInPostOrderIterative((value) => buffer += value)

          buffer should contain inOrderOnly ("A", "C", "E", "D", "B", "H", "I", "G", "F")
        }
      }
    }

    describe("breadth-first search") {
      it("should visit nodes levels by level") {
        val buffer = mutable.ListBuffer.empty[String]
        val tree = anExampleTree()

        tree.traverseBreadthFirst((value) => buffer += value)

        buffer should contain inOrderOnly ("F", "B", "G", "A", "D", "I", "C", "E", "H")
      }
    }
  }

  def anExampleTree(): BinaryTreeNode[String] =
    BinaryTreeNode("F",
      left = BinaryTreeNode("B",
        left = BinaryTreeNode("A"),
        right = BinaryTreeNode("D",
          left = BinaryTreeNode("C"),
          right = BinaryTreeNode("E")
        )
      ),
      right = BinaryTreeNode("G",
        right = BinaryTreeNode("I",
          left = BinaryTreeNode("H")
        )
      )
    )
}



