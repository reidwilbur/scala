import org.scalatest.FunSuite

package ninetynineprobs {

  class p11test extends FunSuite {
    import p11._

    test("rle encode, single els not encoded") {
      val rle = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      println(rle)
      assert(rle == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
    }
  }
}

