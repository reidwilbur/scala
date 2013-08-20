import org.scalatest.FunSuite

package ninetynineprobs {

  class p07test extends FunSuite {
    test("flatten(List(List(1, 1), 2, List(3, List(5, 8))) == List(1, 1, 2, 3, 5, 8)") {
      assert(p07.flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
    }
  }

}
