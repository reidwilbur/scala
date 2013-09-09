import org.scalatest.FunSuite

package ninetynineprobs {
package bintree {

  class p50test extends FunSuite {
    import bintree._

    test("Tree.cBalanced(1, 'x) == List(Tree('x))") {
      val trees = Tree.cBalanced(1, 'x)

      val exp = List(Node('x))

      assert(trees == exp)
    }

    test("Tree.cBalanced(2, 'x)") {
      val trees = Tree.cBalanced(2, 'x)

      val exp = List(
        Node('x, Node('x, End, End), End), 
        Node('x, End, Node('x, End, End))
      )

      assert(exp.length == trees.length)
      exp foreach { assert( trees.contains _ ) }
    }

    test("Tree.cBalanced(3, 'x)") {
      val trees = Tree.cBalanced(3, 'x)

      val exp = List(Node('x, Node('x, End, End), Node('x, End, End)))

      assert(exp.length == trees.length)
      exp foreach { assert( trees.contains _ ) }
    }

    test("Tree.cBalanced(4, 'x)") {
      val trees = Tree.cBalanced(4, 'x)

      val exp = List(
        Node('x, Node('x, Node('x, End, End), End), Node('x, End, End)),
        Node('x, Node('x, End, Node('x, End, End)), Node('x, End, End)),
        Node('x, Node('x, End, End), Node('x, Node('x, End, End), End)),
        Node('x, Node('x, End, End), Node('x, End, Node('x, End, End))),
      )

      assert(exp.length == trees.length)
      exp foreach { assert( trees.contains _ ) }
    }

    test("Tree.cBalanced(5, 'x)") {
      val trees = Tree.cBalanced(5, 'x)

      val exp = List(
        Node('x, Node('x, Node('x, End, End), Node('x, End, End)), Node('x, End,                End)),

        Node('x, Node('x, Node('x, End, End), End),                Node('x, Node('x, End, End), End)),
        Node('x, Node('x, Node('x, End, End), End),                Node('x, End,                Node('x, End, End))),
        
        Node('x, Node('x, End,                Node('x, End, End)), Node('x, Node('x, End, End), End)),
        Node('x, Node('x, End,                Node('x, End, End)), Node('x, End,                Node('x, End, End))),

        Node('x, Node('x, End, Node('x, End, End)), Node('x, End, End)),
        Node('x, Node('x, End, End), Node('x, Node('x, End, End), End)),
        Node('x, Node('x, End, End), Node('x, End, Node('x, End, End))),
      )

      assert(exp.length == trees.length)
      exp foreach { assert( trees.contains _ ) }
    }
  }

}
}

