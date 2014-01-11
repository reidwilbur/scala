
class Node(value: Int) {
  var left:  Option[Node] = None
  var right: Option[Node] = None

  def this(v: Int, left: Option[Node], right: Option[Node]) = {
    this(v)
    this.left = left
    this.right = right
  }

  override def toString: String = {
    value.toString
  }
}

def bst2dll(root: Node): Node = {
  def leftEnd(start: Node): Node = {
    start.left match {
      case Some(node) =>
        leftEnd(node)
      case None =>
        start
    }
  }

  def rightEnd(start: Node): Node = {
    start.right match {
      case Some(node) =>
        rightEnd(node)
      case None =>
        start
    }
  }

  def processInOrder(start:Node): Node = {
    start.left match {
      case Some(node) =>
        val left = rightEnd(processInOrder(node))
        left.right = Some(start)
        start.left = Some(left)
      case None =>
    }
    
    start.right match {
      case Some(node) =>
        val right = processInOrder(node)
        right.left = Some(start)
        start.right = Some(right)
      case None =>
    }

    leftEnd(start)
  }

  processInOrder(root)
}

////////////////////////////////////
// test code

val node13 = new Node(13)
val node14 = new Node(14, Some(node13), None)
val node10 = new Node(10, None, Some(node14))

val node1 = new Node(1)
val node4 = new Node(4)
val node7 = new Node(7)
val node6 = new Node(6, Some(node4), Some(node7))
val node3 = new Node(3, Some(node1), Some(node6))

val node8 = new Node(8, Some(node3), Some(node10))

val dllRoot = bst2dll(node8)

def printDll(start: Option[Node]): Unit = {
  start match {
    case Some(node) =>
      print(node+"-->")
      printDll(node.right)
    case None =>
      println()
  }
}

printDll(Some(dllRoot))

val expOrder = List[Option[Node]](Some(node1), Some(node3), Some(node4), Some(node6), Some(node7), Some(node8), Some(node10), Some(node13), Some(node14))

expOrder.foldLeft(Some(dllRoot): Option[Node]){ 
  (curNode, expNode) => 
    (curNode, expNode) match {
      case (Some(got), Some(exp)) =>
        assert(got eq exp, "got "+got+" exp "+exp)
        got.right
      case (None, None) =>
        None
      case (got, exp) =>
        assert(false, "got "+got+" exp "+exp)
        None
    }
}

