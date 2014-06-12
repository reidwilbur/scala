package fpinscala.datastructures

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TreeSuite extends FunSuite {

  test("25 Tree.size returns correct result") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(5 == Tree.size(t))
  }

  test("26 Tree.maxIntValue returns correct result") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    assert(3 == Tree.maxIntValue(t))
  }

  test("27 Tree.depth returns correct result") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(1))

    assert(2 == Tree.depth(t))

    assert(0 == Tree.depth(Leaf(1)))
  }

  test("28 Tree.map returns correct result") {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

    val s = Tree.map(t)(_.toString)

    assert(Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")) == s)
  }


}
