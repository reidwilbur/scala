package ninetynineprobs

import org.scalatest.FunSuite

class p23test extends FunSuite {
  import p23._

  test("select n random elements from list") {
    val list = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    val r = randomSelect(3, list)
    println(r)
    assert(3 == r.length)

    for(el <- r) assert( list exists { _ == el } )
  }

  test("select n random elements, n > list size") {
    intercept[IllegalArgumentException] {
      randomSelect(10, List('a, 'b, 'c))
    }
  }
}

