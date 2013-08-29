import org.scalatest.FunSuite

package ninetynineprobs {

  class p33test extends FunSuite {
    import S99Int._

    test("35.isCoPrimeTo(64) == true") {
      assert(35.isCoPrimeTo(64))
    }

    test("34.isCoPrimeTo(64) == false") {
      assert(34.isCoPrimeTo(64) == false)
    }

  }

}
