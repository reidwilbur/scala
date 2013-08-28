import org.scalatest.FunSuite

package ninetynineprobs {

  class p32test extends FunSuite {
    import S99Int._

    test("gcd(36, 63) == 9") {
      assert(gcd(36, 63) == 9)
    }

    test("gcd(25, 20) == 5") {
      assert(gcd(25, 20) == 5)
    }

  }

}
