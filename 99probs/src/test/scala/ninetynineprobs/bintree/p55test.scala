package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p55test extends FunSuite {

  test("Tree.cBalanced(1, 'x)") {
    val trees = Tree.cBalanced(1, 'x)

    val exp = List(Node('x))

    assert(trees == exp)
  }

  test("Tree.cBalanced(2, 'x)") {
    val trees = Tree.cBalanced(2, 'x)

    val exp = List(
      Node('x, 
        Node('x), 
        End),

      Node('x, 
        End, 
        Node('x))
    )

    assert(exp.length == trees.length)
    exp.foreach( expTree => { assert(trees.contains(expTree), "Expected tree "+expTree) } )
  }

  test("Tree.cBalanced(3, 'x)") {
    val trees = Tree.cBalanced(3, 'x)

    val exp = List(
      Node('x, 
        Node('x, 
          End, 
          End), 
        Node('x, 
          End, 
          End))
    )

    assert(exp.length == trees.length)
    exp.foreach( expTree => { assert(trees.contains(expTree), "Expected tree "+expTree) } )
  }

  test("Tree.cBalanced(4, 'x)") {
    val trees = Tree.cBalanced(4, 'x)

    val exp = List(
      Node('x, 
        Node('x, 
          Node('x),
          End),                
        Node('x)),

      Node('x, 
        Node('x, 
          End, 
          Node('x)), 
        Node('x)),

      Node('x, 
        Node('x),
        Node('x, 
          Node('x),
          End)),

      Node('x, 
        Node('x),
        Node('x, 
          End, 
          Node('x)))
    )

    assert(exp.length == trees.length)
    exp.foreach( expTree => { assert(trees.contains(expTree), "Expected tree "+expTree) } )
  }

  test("Tree.cBalanced(5, 'x)") {
    val trees = Tree.cBalanced(5, 'x)

    val exp = List(
      Node('x, 
        Node('x, 
          Node('x), 
          End),
        Node('x, 
          Node('x), 
          End)),

      Node('x, 
        Node('x, 
          Node('x), 
          End),
        Node('x, 
          End, 
          Node('x))),
      
      Node('x, 
        Node('x, 
          End, 
          Node('x)), 
        Node('x, 
          End, 
          Node('x))),
      
      Node('x, 
        Node('x, 
          End, 
          Node('x)), 
        Node('x, 
          Node('x),
          End))
    )

    assert(exp.length == trees.length)
    exp.foreach( expTree => { assert(trees.contains(expTree), "Expected tree "+expTree) } )
  }
}

