package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p56test extends FunSuite {

  test("Node('a, Node('b), Node('c)).isSymmetric == true") {
    assert(Node('a, Node('b), Node('c)).isSymmetric == true)
  }

  test("Node('a, Node('b), End).isSymmetric == false") {
    assert(Node('a, Node('b), End).isSymmetric == false)
  }

  test("Node('a, End, Node('b)).isSymmetric == false") {
    assert(Node('a, End, Node('b)).isSymmetric == false)
  }

  test("Node('a, Node('b, Node('c), End), Node('e, Node('f), End)).isSymmetric == false") {
    assert(Node('a, Node('b, Node('c), End), Node('e, Node('f), End)).isSymmetric == false)
  }

  test("Node('a, Node('b, Node('c), End), Node('e, End, Node('f))).isSymmetric == true") {
    assert(Node('a, Node('b, Node('c), End), Node('e, End, Node('f))).isSymmetric == true)
  }

}

