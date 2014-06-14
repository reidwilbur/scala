package fpinscala.errorhandling

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class OptionSuite extends FunSuite {
  test("OOption.map returns correct value") {
    
    assert(SSome(2) == SSome(1).map(i => i+1))

    assert(NNone == NNone.map(_.toString))
  }

  test("OOption.flatMap returns correct value") {

    def plusOneIfLtThree(i: Int): OOption[Int] = {
      if (i < 3) SSome(i+1) else NNone
    }

    assert(SSome(2) == SSome(1).flatMap(plusOneIfLtThree))
    assert(NNone == SSome(3).flatMap(plusOneIfLtThree))
    assert(NNone == NNone.flatMap(plusOneIfLtThree))
  }

  test("OOption.getOrElse returns correct value") {

    assert(2 == SSome(2).getOrElse(0))
    assert(0 == NNone.getOrElse(0))
  }

  test("OOption.orElse returns correct value") {

    assert(SSome(2) == SSome(2).orElse(SSome(3)))
    assert(SSome(3) == NNone.orElse(SSome(3)))
    assert(NNone == NNone.orElse(NNone))
  }

  test("OOption.filter returns correct value") {

    def fltr(i: Int): Boolean = i < 3

    assert(SSome(2) == SSome(2).filter(fltr))
    assert(NNone == SSome(3).filter(fltr))
    assert(NNone == NNone.filter(fltr))
  }
}
