package fpinscala.state

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class StateSuite extends FunSuite {
  class ConstantRNG(value: Int) extends RNG {
    override def nextInt: (Int, RNG) = {
      (value, this)
    }
  }

  test("nonNegative should return Integer.MAX_VALUE instead Integer.MIN_VALUE") {
    assert(Integer.MAX_VALUE == RNG.nonNegativeInt(new ConstantRNG(Integer.MIN_VALUE))._1)
  }

  test("nonNegative should return 0 instead of -1") {
    assert(0 == RNG.nonNegativeInt(new ConstantRNG(-1))._1)
  }

  test("nonNegative should add 1 and invert negative input") {
    assert(25 == RNG.nonNegativeInt(new ConstantRNG(-26))._1)
  }
}
