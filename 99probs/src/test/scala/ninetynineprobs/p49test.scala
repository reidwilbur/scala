package ninetynineprobs

import org.scalatest.FunSuite

class p49test extends FunSuite {
  import S99Logic._

  test("gray(1) == List('0', '1')") {
    val g = gray(1)

    println(g)
    assert(g == List("0", "1"))
  }

  test("gray(2) == List('00', '01', '11' '10')") {
    val g = gray(2)

    println(g)
    assert(g == List("00", "01", "11", "10"))
  }

  test("gray(3) == List('000', '001', '011', '010', '110', '111', '101', '100')") {
    val g = gray(3)

    println(g)
    assert(g == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

  test("grayMemo(1) == List('0', '1')") {
    val g = grayMemo(1)

    println(g)
    assert(g == List("0", "1"))
  }

  test("grayMemo(2) == List('00', '01', '11' '10')") {
    val g = grayMemo(2)

    println(g)
    assert(g == List("00", "01", "11", "10"))
  }

  test("grayMemo(3) == List('000', '001', '011', '010', '110', '111', '101', '100')") {
    val g = grayMemo(3)

    println(g)
    assert(g == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }

}

