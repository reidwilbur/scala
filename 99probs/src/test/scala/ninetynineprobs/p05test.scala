package ninetynineprobs

import org.scalatest.FunSuite

class p05test extends FunSuite {
  test("reverse should return a reversed copy of the input list") {
    val r = p05.reverse(List(1, 1, 2, 3, 5, 8))
    println(r)
    assert(r == List(8, 5, 3, 2, 1, 1))
  }
}

