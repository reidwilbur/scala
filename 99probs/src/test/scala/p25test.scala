import org.scalatest.FunSuite

package ninetynineprobs {

  class p25test extends FunSuite {
    import p25._

    test("random permutation") {
      val l = List('a, 'b, 'c, 'd, 'e, 'f)
      val rp = randomPermute(l)
      
      assert(rp.length == 6)

      rp forall { el => l.exists { _ == el } }
    }

    test("random permutation faster") {
      val l = List('a, 'b, 'c, 'd, 'e, 'f)
      val rp = randomPermuteFast(l)
      
      assert(rp.length == 6)

      rp forall { el => l.exists { _ == el } }
    }
  }
}
