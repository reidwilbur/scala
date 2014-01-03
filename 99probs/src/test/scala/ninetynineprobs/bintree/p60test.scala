package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p60test extends FunSuite {

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

}

