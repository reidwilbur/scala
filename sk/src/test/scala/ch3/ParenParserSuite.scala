package ch3

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ParenParserSuite extends FunSuite {
  import ParenParser._
  
  test("case 1") {
    assert(Left(Success()) == ParenParser.checkParens("()"))
  }
  
  test("case 2") {
    assert(Left(Success()) == ParenParser.checkParens("(()())"))
  }

  test("case 3") {
    assert(Left(Success()) == ParenParser.checkParens("(())"))
  }

  test("case 4") {
    assert(Right(Error(0)) == ParenParser.checkParens(")"))
  }

  test("case 5") {
    assert(Right(Error(0)) == ParenParser.checkParens("("))
  }

  test("case 6") {
    assert(Right(Error(2)) == ParenParser.checkParens("())"))
  }

  test("case 7") {
    assert(Right(Error(2)) == ParenParser.checkParens("()("))
  }
}