package ninetynineprobs {
package bintree {

  sealed abstract class Tree[+T]

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
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
  }

}
}