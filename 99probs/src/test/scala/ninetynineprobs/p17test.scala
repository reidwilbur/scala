package ninetynineprobs

import org.scalatest.FunSuite

class p17test extends FunSuite {
  import p17._

  test("split list at given idx") {
    val sp = split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    println(sp)
    assert(sp == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}

