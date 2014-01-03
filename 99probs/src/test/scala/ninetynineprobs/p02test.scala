package ninetynineprobs

import org.scalatest.FunSuite


class p02test extends FunSuite {
  test("penultimate should return second to last el") {
    val e = p02.penultimate(List(1, 1, 2, 3, 5, 8))
    assert(e == 5)
  }
}

