package algorithms.trees

import scala.collection.mutable

case class Node[T](value: T, left: Node[T] = null, right:Node[T] = null) {

  def traversePreOrderRecursive(visitor: (T) => Unit): Unit = {
    visitor(this.value)

    if (this.left != null) {
      this.left.traversePreOrderRecursive(visitor)
    }

    if (this.right != null) {
      this.right.traversePreOrderRecursive(visitor)
    }
  }

  def traversePreOrder(visitor: (T) => Unit): Unit = {
    val stack = mutable.Stack[Node[T]]()

    stack.push(this)

    while(stack.nonEmpty) {
      val node = stack.pop()

      visitor(node.value)

      if (node.right != null) {
        stack.push(node.right)
      }
      if (node.left != null) {
        stack.push(node.left)
      }
    }
  }


  def traverseInOrderRecursive(visitor: (T) => Unit): Unit = {
    if (this.left != null) {
      this.left.traverseInOrderRecursive(visitor)
    }

    visitor(this.value)

    if (this.right != null) {
      this.right.traverseInOrderRecursive(visitor)
    }
  }

  def traverseInOrder(visitor: (T) => Unit): Unit = {
    val stack = mutable.Stack[Node[T]]()
    var current = this

    while(stack.nonEmpty || current != null) {

      if (current != null) {
        stack.push(current)
        current = current.left
      } else {
        val node = stack.pop()
        visitor(node.value)
        current = node.right
      }
    }
  }
}
