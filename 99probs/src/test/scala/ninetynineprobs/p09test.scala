package ninetynineprobs

import org.scalatest.FunSuite

class p09test extends FunSuite {
  import p09._

  test("packing similar elements of list into packed list") {
    val p = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    
    println(p)
    assert(p == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))

  }
}

