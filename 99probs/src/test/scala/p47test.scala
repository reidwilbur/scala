import org.scalatest.FunSuite

package ninetynineprobs {

  class p47test extends FunSuite {
    import S99Logic._

    test("test table2((a: Boolean, b: Boolean) => a and (a or not(b)))") {
      val t = table2((a: Boolean, b: Boolean) => a and (a or not(b)))
    
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

