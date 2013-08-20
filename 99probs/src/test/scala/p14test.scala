import org.scalatest.FunSuite

package ninetynineprobs {

  class p14test extends FunSuite {
    import p14._

    test("all list els should be duplicated") {
      val dup = duplicate(List('a, 'b, 'c, 'c, 'd))
      println(dup)
      assert(dup == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    }

    test("all list els should be duplicated (flat map impl") {
      val fdup = dupflatmap(List('a, 'b, 'c, 'c, 'd))
      assert(fdup == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
    }
  }
}
