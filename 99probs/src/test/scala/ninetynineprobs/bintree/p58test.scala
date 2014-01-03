package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p58test extends FunSuite {

  test("Tree.symmetricBalancedTrees(4, 'x)") {
    assert(Tree.symmetricBalancedTrees(4, 'x) == Nil)
  }

  test("Tree.symmetricBalancedTrees(6, 'x)") {
    assert(Tree.symmetricBalancedTrees(6, 'x) == Nil)
  }

  test("Tree.symmetricBalancedTrees(5, 'x)") {
    val trees = Tree.symmetricBalancedTrees(5, 'x)

    val exp = List(
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
          Node('x),
          End))
    )

    assert(trees.length == exp.length)
    println("Got:")
    println(trees)
    println("Exp:")
    println(exp)
    exp.foreach( expTree => assert(trees.contains(expTree)) )
  }
}

