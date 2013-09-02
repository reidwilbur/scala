import org.scalatest.FunSuite

package ninetynineprobs {

  class p46test extends FunSuite {
    import S99Logic._

    test("and(true, true) == true") {
      assert(and(true, true) == true)
    }

    test("xor(true, false) == true") {
      assert(xor(true, false) == true)
    }

    test("table2") {
      val t = table2((a: Boolean, b: Boolean) => and(a, or(a, b)))

      val exp = List(
        "A     B     result",
        "false false false",
        "false true  false",
        "true  false true",
        "true  true  true"
      )
      println("Got:")
      println(t.mkString("\n"))
      println("Expected:")
      println(exp.mkString("\n"))
      
      assert(t == exp)
    }
  }

}


