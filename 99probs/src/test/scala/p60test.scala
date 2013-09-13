import org.scalatest.FunSuite

package ninetynineprobs {
package bintree {

  class p60test extends FunSuite {
    import bintree._

    test("Tree.hbalTreesWithNodes(4, 'x)") {
      val trees = Tree.hbalTreesWithNodes(4, 'x)

      val exp = List(
        Node('x,
          Node('x,
            Node('x),
            End),
          Node('x)
        ),

        Node('x,
          Node('x,
            End, 
            Node('x)),
          Node('x)
        ),

        Node('x,
          Node('x),
          Node('x,
            Node('x),
            End)
        ),

        Node('x,
          Node('x),
          Node('x,
            End, 
            Node('x))
        )

      )

      println("Got:")
      println(trees)
      println("Exp:")
      println(exp)

      assert(exp.length == trees.length)
      exp.foreach( expTree => assert( trees.contains(expTree) ) )
    }

    test("Tree.hbalTreesWithNodes(15, 'x)") {
      val trees = Tree.hbalTreesWithNodes(15, 'x)
      println("Tree.hbalTreesWithNodes(15, 'x).length = "+trees.length)
      println(trees)
    }

  }

}
}

