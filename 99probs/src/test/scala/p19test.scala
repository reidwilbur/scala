import org.scalatest.FunSuite

package ninetynineprobs {

  class p19test extends FunSuite {
    import p19._

    test("rotate list positive (left)") {
      val l = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      assert(l == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    }

    test("rotate list negative (right)") {
      val r = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
      assert(r == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
    }

    test("rotate list positive greater than length (left)") {
      val s = rotate(5, List('a, 'b, 'c, 'd))
      assert(s == List('b, 'c, 'd, 'a))
    }
    
    test("rotate list negative greater than length (right)") {
      val t = rotate(-6, List('a, 'b, 'c, 'd))
      assert(t == List('c, 'd, 'a, 'b))
    }
    
  }
}


