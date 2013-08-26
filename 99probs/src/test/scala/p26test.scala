import org.scalatest.FunSuite

package ninetynineprobs {

  class p26test extends FunSuite {
    import p26._

    test("C(4,3)") {
      val l = List('a, 'b, 'c, 'd)
      val c = combinations(3, l)
      
      assert(c.length == 4)

      val expected = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'c, 'd), List('b, 'c, 'd))
      expected.foreach { combi => assert ( c.exists { _ == combi } ) }
    }

    test("C(4,2)") {
      val l = List('a, 'b, 'c, 'd)
      val c = combinations(2, l)
      
      assert(c.length == 6)

      val expected = List(
        List('a, 'b), 
        List('a, 'c), 
        List('a, 'd),
        List('b, 'c),
        List('b, 'd),
        List('c, 'd)
      )
      expected.foreach { combi => assert ( c.exists { _ == combi } ) }
    }

    test("C(5,3)") {
      val l = List('a, 'b, 'c, 'd, 'e)
      val c = combinations(3, l)
      
      assert(c.length == 10)

      val expected = List(
        List('a, 'b, 'c),
        List('a, 'b, 'd),
        List('a, 'b, 'e),
        List('a, 'c, 'd),
        List('a, 'c, 'e),
        List('a, 'd, 'e),
        List('b, 'c, 'd),
        List('b, 'c, 'e),
        List('b, 'd, 'e),
        List('c, 'd, 'e)
      )
      expected.foreach { combi => assert ( c.exists { _ == combi } ) }
    }
  }

}
