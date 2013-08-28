import org.scalatest.FunSuite

package ninetynineprobs {

  class p31test extends FunSuite {
    import S99Int._

    test("7.isPrime == true") {
      assert(7.isPrime)
    }

    test("9.isPrime == false") {
      assert(9.isPrime == false)
    }

    test("1.isPrime == true") {
      assert(1.isPrime)
    }

    test("0.isPrime == false") {
      assert(0.isPrime == false)
    }

    test("-2.isPrime == true") {
      assert(-2.isPrime)
    }

    test("-24.isPrime == false") {
      assert(-24.isPrime == false)
    }

  }

}
