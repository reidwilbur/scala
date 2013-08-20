import org.scalatest.FunSuite

package ninetynineprobs {

  class p16test extends FunSuite {
    import p16._

    test("drop el of list with given idx (0 based)") {
      val d = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      println(d)
      assert(d == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
    }
  }
}


