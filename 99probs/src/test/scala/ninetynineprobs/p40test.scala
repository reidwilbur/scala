package ninetynineprobs

import org.scalatest.FunSuite

class p40test extends FunSuite {
  import S99Int._

  test("28.goldbach == (5,23)") {
    assert(28.goldbach == (5,23))
  }

}

