package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p57test extends FunSuite {

  test("End.addValue(2)") {
    assert(End.addValue(2) == Node(2))
  }

  test("End.addValue(2).addValue(3)") {
    val t = End.addValue(2).addValue(3)
    assert(t == Node(2, End, Node(3)))
  }

  test("End.addValue(2).addValue(3).addValue(0)") {
    val t = End.addValue(2).addValue(3).addValue(0)
    assert(t == Node(2, Node(0), Node(3)))
  }

  test("Tree.fromList(List(3, 2, 5, 7, 1))") {
    val t = Tree.fromList(List(3, 2, 5, 7, 1))
    assert(t == Node(3, Node(2, Node(1), End), Node(5, End, Node(7))))
  }

  test("Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric") {
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric == true)
  }

  test("Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric") {
    assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric == false)
  }
}

