import org.scalatest.FunSuite

package ninetynineprobs {

  class p20test extends FunSuite {
    import p20._

    test("test remove at index (0 based)") {
      val r = removeAt(1, List('a, 'b, 'c, 'd))
      println(r)
      assert(r == (List('a, 'c, 'd),'b))
    }
  }
}
