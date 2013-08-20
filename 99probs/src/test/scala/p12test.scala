import org.scalatest.FunSuite

package ninetynineprobs {

  class p12test extends FunSuite {
    import p12._

    test("decode rle packed list") {
      val decoded = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) 
      println(decoded)
      assert(decoded == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    }

  }
}
