package ninetynineprobs.bintree

import org.scalatest.FunSuite

class p62test extends FunSuite {

  test("Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList") {
    val nodes = Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList

    val exp = List('e', 'd', 'b')

    println("Got:")
    println(nodes)
    println("Exp:")
    println(exp)
    assert(nodes == exp)
  }

  test("Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList") {
    val nodes = Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList

    val exp = List('c', 'a')

    println("Got:")
    println(nodes)
    println("Exp:")
    println(exp)
    assert(nodes == exp)
  }

  test("Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)") {
    val nodes = Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2)

    val exp = List('c', 'b')

    println("Got:")
    println(nodes)
    println("Exp:")
    println(exp)
    assert(nodes == exp)
  }
}

