import org.scalatest.FunSuite

package ninetynineprobs {
package bintree {

  class p59test extends FunSuite {
    import bintree._

    test("Trees.hBalanced(3, 'x)") {
      val trees = Tree.hBalanced(3, 'x)

      val exp = List(
        Node('x,
          Node('x,
            Node('x), 
            Node('x)),
          Node('x,
            Node('x),
            Node('x))
        ),

        Node('x,
          Node('x,
            End, 
            Node('x)),
          Node('x,
            Node('x),
            Node('x))
        ),

        Node('x,
          Node('x,
            Node('x), 
            End),
          Node('x,
            Node('x),
            Node('x))
        ),

        Node('x,
          Node('x,
            Node('x), 
            Node('x)),
          Node('x,
            End,
            Node('x))
        ),

        Node('x,
          Node('x,
            Node('x), 
            Node('x)),
          Node('x,
            Node('x),
            End)
        ),

        Node('x,
          Node('x),
          Node('x,
            Node('x),
            Node('x))
        ),

        Node('x,
          Node('x),
          Node('x,
            End,
            Node('x))
        ),

        Node('x,
          Node('x),
          Node('x,
            Node('x),
            End)
        ),

        Node('x,
          Node('x,
            Node('x), 
            Node('x)),
          Node('x)
        ),

        Node('x,
          Node('x,
            End, 
            Node('x)),
          Node('x)
        ),

        Node('x,
          Node('x,
            Node('x), 
            End),
          Node('x)
        )
      )

      println("Got:")
      println(trees)
      println("Exp:")
      println(exp)
      assert(exp.length == trees.length)
      exp.foreach{ exptree => assert( trees.contains(exptree) ) }
    }
  }

}
}
