
sealed abstract class Tree[+T] {
  def isMirrorOf[S](tree: Tree[S]): Boolean;
  def isSymmetric: Boolean;
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U];
  def nodeCount: Int;
  def height: Int;
  def leafCount: Int;
  def leafList: List[T];
  def internalList: List[T]
  def atLevel(level: Int): List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  val nodeCount = left.nodeCount + right.nodeCount + 1
  val height = math.max(left.height, right.height) + 1
  val leafCount = (left, right) match {
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  override def isMirrorOf[S](tree: Tree[S]): Boolean =
    tree match {
      case Node(_, left, right) =>
        this.left.isMirrorOf(right) && this.right.isMirrorOf(left)
      case _ =>
        false
    }

  override def isSymmetric: Boolean = left.isMirrorOf(right)

  override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
    x match {
      case _ if (x > this.value) =>
        Node(this.value, this.left, this.right.addValue(x))
      case _ =>
        Node(this.value, this.left.addValue(x), this.right)
    }
  }

  override def leafList: List[T] = {
    def leafListTR(leafNodes: List[T], workingNodes: List[Tree[T]]): List[T] = 
      workingNodes match {
        case Nil => 
          leafNodes

        case Node(v, l, r) :: rest =>
          val (nextLeaves, nextWorking) = 
            (v, l, r) match { 
              case (_, End, End) => (v :: leafNodes, rest)
              case _             => (leafNodes, l :: r :: rest)
            }
          leafListTR(nextLeaves, nextWorking)

        case _ :: rest =>
          leafListTR(leafNodes, rest)
      }

    leafListTR(Nil, this :: Nil)
  }

  override def internalList: List[T] = {
    def internalListTR(internalNodes: List[T], workingNodes: List[Tree[T]]): List[T] =
      workingNodes match {
        case Nil => 
          internalNodes

        case Node(v, l, r) :: rest =>
          val (nextInternal, nextWorking) =
            (v, l, r) match {
              case (_, End, End) => (internalNodes, rest)
              case _             => (v :: internalNodes, l :: r :: rest)
            }
          internalListTR(nextInternal, nextWorking)

        case _ :: rest =>
          internalListTR(internalNodes, rest)
      }

    internalListTR(Nil, this :: Nil)
  }

  override def atLevel(level: Int): List[T] = {
    def atLevelTR(levelNodes: List[T], workingNodes: List[(Int, Tree[T])]): List[T] =
      workingNodes match {
        case Nil =>
          levelNodes

        case (nodeLevel, Node(v, _, _)) :: rest if (nodeLevel == level) =>
          atLevelTR(v :: levelNodes, rest)

        case (nodeLevel, Node(_, l, r)) :: rest =>
          atLevelTR(levelNodes, (nodeLevel+1, l) :: (nodeLevel+1, r) :: rest)

        case _ :: rest =>
          atLevelTR(levelNodes, rest)
      }

    atLevelTR(Nil, (1, this) :: Nil)
  }

  def layoutBinaryTree: Tree[T] = {
    def inOrderDfs(node: Tree[T], idx: Int, lvl: Int): (Tree[T], Int) = {
      node match {
        case End => (End, idx)
        case Node(v, l, r) =>
          val (left, leftIdx) = inOrderDfs(l, idx, lvl+1)
          val x = leftIdx + 1
          val (right, rightIdx) = inOrderDfs(r, x, lvl+1)
          (PositionedNode(v, left, right, x, lvl), rightIdx)
      }
    }

    inOrderDfs(this, 0, 1)._1
  }
}

case class PositionedNode[+T](
  value: T, 
  left: Tree[T], 
  right: Tree[T], 
  x: Int, 
  y: Int) 
extends Tree[T] {
  val node = Node(value, left, right)
  
  def isMirrorOf[S](tree: Tree[S]): Boolean = node.isMirrorOf(tree)
  
  def isSymmetric: Boolean = node.isSymmetric
  
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = throw new RuntimeException("not supported")
  
  def nodeCount: Int = node.nodeCount
  
  def height: Int = node.height
  
  def leafCount: Int = node.leafCount
  
  def leafList: List[T] = node.leafList
  
  def internalList: List[T] = node.internalList
  
  def atLevel(level: Int): List[T] = node.atLevel(level)

  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."

  override def isMirrorOf[S](tree: Tree[S]): Boolean = {
    tree match {
      case End => true
      case _ => false
    }
  }

  override def isSymmetric = true

  override def addValue[U <% Ordered[U]](x: U): Tree[U] = Node(x)

  override def nodeCount = 0

  override def height = 0

  override def leafCount = 0

  override def leafList = Nil

  override def internalList = Nil

  override def atLevel(level: Int) = Nil
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def cBalanced[S](nodes: Int, value: S): List[Tree[S]] = {
    nodes match {
      //create the easy base cases
      case 0 => 
        Nil
      case 1 => 
        List(Node(value))
      case 2 => 
        List(
          Node(value, Node(value), End), 
          Node(value, End,         Node(value))
        )
      case n if (n % 2 == 0) =>
        //the number of nodes we are adding to the tree must add up to n
        // n = n/2 + (n-1)/2 + 1
        // 1 is the new node we are adding at this level
        // n/2 and (n-1)/2 are the sub trees that exist below this new node
        val subtreesHalf = cBalanced(n/2, value)
        val subtreesHalfLessOne = cBalanced((n-1)/2, value)
        //create all the combinations of trees for this this node and
        //the subtree lists computed above
        val halfLeft = 
          for(l <- subtreesHalf; r <- subtreesHalfLessOne) yield(Node(value,l,r))
        val halfLessOneLeft = 
          for(l <- subtreesHalfLessOne; r <- subtreesHalf) yield(Node(value,l,r))
        //return all the possible trees
        halfLeft ::: halfLessOneLeft
      case n =>
        //this case is easier than the above case since we have an odd number of
        //nodes, n-1 is divisible by 2 and is symmetric so only have to compute
        //one set of sub trees
        val subtrees = cBalanced((n-1)/2, value)
        //create each tree as each combination of all possible sub trees and this 
        //new node
        for(l <- subtrees; r <- subtrees) yield(Node(value, l, r))
    }
  }

  def fromList[S <% Ordered[S]](list: List[S]): Tree[S] = {
    list.foldLeft(End: Tree[S])( (t, el) => { t.addValue(el) } )
  }

  def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] = {
    cBalanced(nodes, value).filter( _.isSymmetric )
  }

  def hBalanced[T](height: Int, value: T): List[Tree[T]] = {
    height match {
      case 0 => 
        Nil
      case 1 =>
        List(
          Node(value)
        )
      case 2 =>
        List(
          Node(value, Node(value), End), 
          Node(value, End, Node(value)), 
          Node(value, Node(value), Node(value))
        )
      case _ =>
        val subtrees1 = hBalanced(height-1, value)
        val subtrees2 = hBalanced(height-2, value)
        val sameHeight = 
          for(l <- subtrees1; r <- subtrees1) yield(Node(value, l, r))
        val longLeft =
          for(l <- subtrees1; r <- subtrees2) yield(Node(value, l, r))
        val longRight =
          for(r <- subtrees1; l <- subtrees2) yield(Node(value, l, r))
        sameHeight ::: longLeft ::: longRight
    }
  }

  def maxHbalHeight(nodes: Int): Int = {
    math.ceil((nodes-1)/2.0).toInt + 1
  }

  def minHbalHeight(nodes: Int): Int = {
    math.floor(math.log(nodes)/math.log(2)).toInt + 1
  }

  def hbalTreesWithNodes[S](nodes: Int, value: S): List[Tree[S]] = {
    for{
      h <- (minHbalHeight(nodes) to maxHbalHeight(nodes)).toList
      t <- hBalanced(h, value)
      if t.nodeCount == nodes
    }
    yield t
  }

  def completeBinaryTree[S](nodes: Int, value: S): Tree[S] = {
    def genParentNodes(parNodes: List[Tree[S]], childNodes: List[Tree[S]]): List[Tree[S]] = {
      childNodes match {
        case Nil =>
          parNodes.reverse
        case left :: right :: rest =>
          genParentNodes(Node(value, left, right) :: parNodes, rest)
        case _ =>
          throw new RuntimeException("childNodes list length not a power of 2")
      }
    }

    def cBinTreeTR(childNodes: List[Tree[S]]): Tree[S] = {
      childNodes match {
        case h :: Nil =>
          h
        case _ =>
          val parNodes = genParentNodes(Nil, childNodes)
          cBinTreeTR(parNodes)
      }
    }

    def genLeafNodes(currNodeCnt: Int, nextNodesCnt: Int): List[Tree[S]] =
      if ((nodes >= currNodeCnt) && (nodes <= currNodeCnt + nextNodesCnt)) {
        val endNodeCnt = currNodeCnt + nextNodesCnt - nodes
        val nodeCnt = nodes - currNodeCnt
        List.fill(nodeCnt)(Node(value)) ::: List.fill(endNodeCnt)(End)
      }
      else
        genLeafNodes(currNodeCnt+nextNodesCnt, nextNodesCnt*2)

    val leaves = genLeafNodes(1, 2)

    cBinTreeTR(leaves)
  }

}
