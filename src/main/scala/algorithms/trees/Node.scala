package algorithms.trees

import scala.collection.mutable

case class Node[T](value: T, left: Node[T] = null, right:Node[T] = null) {

  def traversePreOrder(visitor: (T) => Unit) = {
    val stack: mutable.Stack[Node[T]] = mutable.Stack()

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
}
