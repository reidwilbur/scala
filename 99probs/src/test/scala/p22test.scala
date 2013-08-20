import org.scalatest.FunSuite

package ninetynineprobs {

  class p22test extends FunSuite {
    import p22._

    test("create range") {
      val r = range(4, 9)
      println(r)
      assert(r == List(4, 5, 6, 7, 8, 9))
    }

    test("create invalid range") {
      intercept[IllegalArgumentException] {
        range(4, 1)
      }
    }
  }
}
