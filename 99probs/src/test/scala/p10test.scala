import org.scalatest.FunSuite

package ninetynineprobs {

  class p10test extends FunSuite {
    import p10._

    test("rle encode list") {
      val rle = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
      println(rle)
      assert(rle == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
    }
  }
}
