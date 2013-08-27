import org.scalatest.FunSuite

package ninetynineprobs {

  class p28test extends FunSuite {
    import p28._

    test("test lsort") {
      val l = List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))
      val sl = lsort(l)

      assert(sl == List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)))
    }

  }

}
