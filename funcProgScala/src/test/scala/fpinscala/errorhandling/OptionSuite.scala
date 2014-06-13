package fpinscala.errorhandling

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class OptionSuite extends FunSuite {
  test("MyOption.map returns correct value") {
    
    assert(MySome(2) == MySome(1).map(i => i+1))

    assert(MyNone == MyNone.map(_.toString))
  }

  test("MyOption.flatMap returns correct value") {

    def plusOneIfLtThree(i: Int): MyOption[Int] = {
      if (i < 3) MySome(i+1) else MyNone
    }

    assert(MySome(2) == MySome(1).flatMap(plusOneIfLtThree))
    assert(MyNone == MySome(3).flatMap(plusOneIfLtThree))
    assert(MyNone == MyNone.flatMap(plusOneIfLtThree))
  }

  test("MyOption.getOrElse returns correct value") {

    assert(2 == MySome(2).getOrElse(0))
    assert(0 == MyNone.getOrElse(0))
  }

  test("MyOption.orElse returns correct value") {

    assert(MySome(2) == MySome(2).orElse(MySome(3)))
    assert(MySome(3) == MyNone.orElse(MySome(3)))
    assert(MyNone == MyNone.orElse(MyNone))
  }

  test("MyOption.filter returns correct value") {

    def fltr(i: Int): Boolean = i < 3

    assert(MySome(2) == MySome(2).filter(fltr))
    assert(MyNone == MySome(3).filter(fltr))
    assert(MyNone == MyNone.filter(fltr))
  }
}
