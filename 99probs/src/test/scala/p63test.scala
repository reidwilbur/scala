import org.scalatest.FunSuite

package ninetynineprobs {
package bintree {

  class p63test extends FunSuite {
    import bintree._

    test("Tree.completeBinaryTree(6, 'x)") {
      val t = Tree.completeBinaryTree(6, 'x)

      val exp = Node('x, Node('x, Node('x), Node('x)), Node('x, Node('x), End))

      println("Got:")
      println(t)
      println("Exp:")
      println(exp)

      assert(exp == t)
    }

    test("Tree.completeBinaryTree(5, 'x)") {
      val t = Tree.completeBinaryTree(5, 'x)

      val exp = Node('x, Node('x, Node('x), Node('x)), Node('x))

      println("Got:")
      println(t)
      println("Exp:")
      println(exp)

      assert(exp == t)
    }

    test("Tree.completeBinaryTree(7, 'x)") {
      val t = Tree.completeBinaryTree(7, 'x)

      val exp = Node('x, Node('x, Node('x), Node('x)), Node('x, Node('x), Node('x)))

      println("Got:")
      println(t)
      println("Exp:")
      println(exp)

      assert(exp == t)
    }

    test("Tree.completeBinaryTree(8, 'x)") {
      val t = Tree.completeBinaryTree(8, 'x)

      val exp = Node('x, Node('x, Node('x, Node('x), End), Node('x)), Node('x, Node('x), Node('x)))

      println("Got:")
      println(t)
      println("Exp:")
      println(exp)

      assert(exp == t)
    }

    test("Tree.completeBinaryTree(4, 'x)") {
      val t = Tree.completeBinaryTree(4, 'x)

      val exp = Node('x, Node('x, Node('x), End), Node('x, End, End))

      println("Got:")
      println(t)
      println("Exp:")
      println(exp)

      assert(exp == t)
    }
  }

}
}
