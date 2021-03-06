package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p64test extends FunSuite {

  test("Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree") {
    val t = Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree

    val exp = 
      PositionedNode('a', 
        PositionedNode('b', 
          End, 
          PositionedNode('c', 
            End, 
            End, 
            2, 
            3), 
          1,
          2),
        PositionedNode('d',
          End, 
          End, 
          4, 
          2),
        3,
        1)


    println("Got: ")
    println(t)
    println("Exp: ")
    println(exp)

    assert(exp == t)
  }

}

