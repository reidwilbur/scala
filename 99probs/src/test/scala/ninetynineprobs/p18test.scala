package ninetynineprobs

import org.scalatest.FunSuite

class p18test extends FunSuite {
  import p18._

  test("return a slice of a list") {
    val s = slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) 
    println(s)
    assert(s == List('d, 'e, 'f, 'g))
  }
}

