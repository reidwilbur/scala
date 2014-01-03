package ninetynineprobs

import org.scalatest.FunSuite

class p01test extends FunSuite {
  test("last should return last element of the list") {
    val l = p01.last(List(1, 1, 2, 3, 5, 8))
    assert(8 == l) 
  }
}

