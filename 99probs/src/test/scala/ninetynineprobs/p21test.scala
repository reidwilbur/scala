package ninetynineprobs

import org.scalatest.FunSuite

class p21test extends FunSuite {
  import p21._

  test("insert with index < max index") {
    val i = insertAt('new, 1, List('a, 'b, 'c, 'd))
    println(i)
    assert(i == List('a, 'new, 'b, 'c, 'd))
  }

  test("insert with index > max index") {
    val j = insertAt('new, 32, List('a, 'b, 'c, 'd))
    println(j)
    assert(j == List('a, 'b, 'c, 'd, 'new))
  }
}

