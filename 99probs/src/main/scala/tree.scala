package ninetynineprobs {
package bintree {

  sealed abstract class Tree[+T] {
    def isMirrorOf[S](tree: Tree[S]): Boolean;
    def isSymmetric: Boolean;
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U];
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
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

    def minHbalNodes(height: Int): Int = {
      height match {
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case _ =>
          val minH1 = minHbalNodes(height-1)
          val minH2 = minHbalNodes(height-2)
          minH1+minH2+1
      }
    }

  }

}
}
