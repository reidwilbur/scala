import org.scalatest.FunSuite

package ninetynineprobs {

  class p39test extends FunSuite {
    import S99Int._

    test("listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31)") {
      assert(listPrimesinRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))
    }

  }

}

