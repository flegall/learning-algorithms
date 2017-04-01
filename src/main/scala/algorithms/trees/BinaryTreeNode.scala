package algorithms.trees

import scala.collection.mutable

case class BinaryTreeNode[T](value: T,
                             left: Option[BinaryTreeNode[T]] = None,
                             right: Option[BinaryTreeNode[T]] = None) {

  def traversePreOrderRecursive(visitor: (T) => Unit): Unit = {
    visitor(this.value)

    this.left.foreach(_.traversePreOrderRecursive(visitor))
    this.right.foreach(_.traversePreOrderRecursive(visitor))
  }

  def traversePreOrderIterative(visitor: (T) => Unit): Unit = {
    val stack = mutable.Stack[BinaryTreeNode[T]]()

    stack.push(this)

    while(stack.nonEmpty) {
      val node = stack.pop()

      visitor(node.value)

      node.right.foreach(stack.push)
      node.left.foreach(stack.push)
    }
  }


  def traverseInOrderRecursive(visitor: (T) => Unit): Unit = {
    this.left.foreach(_.traverseInOrderRecursive(visitor))

    visitor(this.value)

    this.right.foreach(_.traverseInOrderRecursive(visitor))
  }

  def traverseInOrderIterative(visitor: (T) => Unit): Unit = {
    val stack = mutable.Stack[BinaryTreeNode[T]]()
    var current: Option[BinaryTreeNode[T]] = Some(this)

    while(stack.nonEmpty || current.nonEmpty) {

      current match {
        case Some(node) =>
          stack.push(node)
          current = node.left
        case None =>
          val node = stack.pop()
          visitor(node.value)
          current = node.right
      }
    }
  }

  def traverseInPostOrderRecursive(visitor: (T) => Unit): Unit = {
    this.left.foreach(_.traverseInPostOrderRecursive(visitor))
    this.right.foreach(_.traverseInPostOrderRecursive(visitor))

    visitor(this.value)
  }

  def traverseInPostOrderIterative(visitor: (T) => Unit): Unit = {
    val stack = mutable.Stack[BinaryTreeNode[T]]()
    var head = this
    stack.push(head)

    while (stack.nonEmpty) {
      val next = stack.head

      val finishedSubtrees = next.right.contains(head) || next.left.contains(head)
      val isLeaf = next.left.isEmpty && next.right.isEmpty

      if (finishedSubtrees || isLeaf) {
        stack.pop()
        visitor(next.value)
        head = next
      } else {
        next.right.foreach(node => stack.push(node))
        next.left.foreach(node => stack.push(node))
      }
    }
  }

  def traverseBreadthFirst(visitor: (T) => Unit): Unit = {
    val queue = mutable.Queue[BinaryTreeNode[T]]()

    queue.enqueue(this)

    while(queue.nonEmpty) {
      val node = queue.dequeue()

      visitor(node.value)

      node.left.foreach(node => queue.enqueue(node))
      node.right.foreach(node => queue.enqueue(node))
    }
  }
}
