package ninetynineprobs

import org.scalatest.FunSuite

class p41test extends FunSuite {
  import S99Int._

  test("getGoldbachList") {
    val sums = getGoldbachList(9 to 20)
    val exp = 
      List(
        GoldbachSum(10, (3, 7)),
        GoldbachSum(12, (5, 7)),
        GoldbachSum(14, (3, 11)),
        GoldbachSum(16, (3, 13)),
        GoldbachSum(18, (5, 13)),
        GoldbachSum(20, (3, 17))
      )

    println("Got:")
    sums foreach { println _ }
    println("Expected:")
    exp foreach { println _ }

    assert(sums == exp)
  }

  test("getGoldbachListLimited") {
    val sums = getGoldbachListLimited(1 to 2000, 50)
    
    println("Got:")
    sums foreach { println _ }
  }

}

