import org.scalatest.FunSuite

package ninetynineprobs {

  class p37test extends FunSuite {
    import S99Int._

    test("10.totientFromPrimeFactors == 4") {
      assert(10.totient == 4)
    }

  }

}

