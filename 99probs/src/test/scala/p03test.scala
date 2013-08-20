import org.scalatest.FunSuite

package ninetynineprobs {

  class p03test extends FunSuite {
    test("nth should return the nth element 0 based") {
      val n = p03.nth(2, List(1, 1, 2, 3, 5, 8)) 
      println(n)
      assert(n == 2)
    }
  }

}

