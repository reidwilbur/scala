package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p61test extends FunSuite {
  
  test("Node('x, Node('x), End).leafCount") {
    assert(Node('x, Node('x), End).leafCount == 1)
  }
  
  test("Node('x, Node('x), Node('x)).leafCount") {
    assert(Node('x, Node('x), Node('x)).leafCount == 2)
  }
  
  test("Node('x, Node('x), Node('x, Node('x), End)).leafCount") {
    assert(Node('x, Node('x), Node('x, Node('x), End)).leafCount == 2)
  }
  
  test("Node('x, End, Node('x, Node('x), Node('x))).leafCount") {
    assert(Node('x, End, Node('x, Node('x), Node('x))).leafCount == 2)
  }
}

