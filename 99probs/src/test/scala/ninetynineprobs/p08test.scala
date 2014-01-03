package ninetynineprobs

import org.scalatest.FunSuite

class p08test extends FunSuite {
  import p08._

  test("compress(Nil) == Nil") {
    assert(compress(Nil) == Nil)
  }

  test("compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e)") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("compress(List('b)) == List('b)") {
    assert(compress(List('b)) == List('b))
  }
}

