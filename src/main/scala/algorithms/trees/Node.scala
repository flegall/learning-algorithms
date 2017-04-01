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

  def traversePreOrderIterative(visitor: (T) => Unit): Unit = {
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

  def traverseInOrderIterative(visitor: (T) => Unit): Unit = {
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

  def traverseInPostOrderRecursive(visitor: (T) => Unit): Unit = {
    if (this.left != null) {
      this.left.traverseInPostOrderRecursive(visitor)
    }

    if (this.right != null) {
      this.right.traverseInPostOrderRecursive(visitor)
    }

    visitor(this.value)
  }

  def traverseInPostOrderIterative(visitor: (T) => Unit): Unit = {
    val stack = mutable.Stack[Node[T]]()
    var head = this
    stack.push(head)

    while (stack.nonEmpty) {
      val next = stack.head

      val finishedSubtrees = next.right == head || next.left == head
      val isLeaf = next.left == null && next.right == null

      if (finishedSubtrees || isLeaf) {
        stack.pop()
        visitor(next.value)
        head = next
      } else {
        if (next.right != null) {
          stack.push(next.right)
        }
        if (next.left != null) {
          stack.push(next.left)
        }
      }
    }
  }
}
