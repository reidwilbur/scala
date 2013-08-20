import org.scalatest.FunSuite

package ninetynineprobs {

  class p04test extends FunSuite {
    test("length should return the lenght of the list") {
      val l = p04.length(List(1, 1, 2, 3, 5, 8)) 
      println(l)
      assert(l == 6)
    }
  }

}


