import org.scalatest.FunSuite

package ninetynineprobs {

  class p36test extends FunSuite {
    import S99Int._

    test("315.primeFactorMultiplicity == Map(3 -> 2, 5 -> 1, 7 -> 1)") {
      assert(315.primeFactorMultiplicity == Map(3 -> 2, 5 -> 1, 7 -> 1))
    }

    test("17.primeFactorMultiplicity == Map(17 -> 1)") {
      assert(17.primeFactorMultiplicity == Map(17 -> 1))
    }

  }

}
