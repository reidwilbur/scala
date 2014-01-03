package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p59test extends FunSuite {

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
          Node('x), 
          End),
        Node('x,
          End,
          Node('x))
      ),

      Node('x,
        Node('x,
          End, 
          Node('x)),
        Node('x,
          Node('x),
          End)
      ),

      Node('x,
        Node('x,
          Node('x), 
          End),
        Node('x,
          Node('x),
          End)
      ),

      Node('x,
        Node('x,
          End, 
          Node('x)),
        Node('x,
          End,
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

