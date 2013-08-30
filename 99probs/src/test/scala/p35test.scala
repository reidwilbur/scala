import org.scalatest.FunSuite

package ninetynineprobs {

  class p35test extends FunSuite {
    import S99Int._

    test("315.primeFactors == List(3, 3, 5, 7)") {
      assert(315.primeFactors == List(3, 3, 5, 7))
    }

    test("17.primeFactors == List(17)") {
      assert(17.primeFactors == List(17))
    }

  }

}
