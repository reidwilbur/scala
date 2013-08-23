import org.scalatest.FunSuite

package ninetynineprobs {

  class p26test extends FunSuite {
    import p26._

    test("random permutation") {
      val l = List('a, 'b, 'c, 'd)
      val c = combinations(2, l)
      
      assert(c.length == 6)

      val expected = List(List('a, 'b), List('a, 'c), List('a, 'd), List('b, 'c), List('b, 'd), List('c, 'd))
      assert( expected forall { combination => c exists { _ == combination }  } )
    }
  }

}
