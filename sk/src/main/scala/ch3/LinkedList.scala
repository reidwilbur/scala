package ch3

object LinkedList {
  class Node[T](val value: T) {
    var next: Option[Node[T]] = None
    
    def this(value: T, next: Node[T]) = {
      this(value)
      this.next = Some(next)
    }
  }
  
  def reverseList[T](head: Node[T]): Node[T] = {
    def rev(left: Node[T], right: Option[Node[T]]): Node[T] = {
      right match {
        case None =>
          left
        case Some(node) =>
          val right = node.next
          node.next = Some(left)
          rev(node, right)
      }
    }
    val tmp = head.next
    head.next = None
    rev(head, tmp)
  }
}