package ninetynineprobs

import org.scalatest.FunSuite

class p15test extends FunSuite {
  import p15._

  test("make n copies of east list el") {
    val dupn = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
    println(dupn)
    assert(dupn == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
}

