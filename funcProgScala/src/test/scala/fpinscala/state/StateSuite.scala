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

  test("nonNegative should return Integer.MAX_VALUE instead Integer.MIN_VALUE")
  {
    assert(Integer.MAX_VALUE == RNG.nonNegativeInt(new ConstantRNG(Integer.MIN_VALUE))._1)
  }

  test("nonNegative should return 0 instead of -1")
  {
    assert(0 == RNG.nonNegativeInt(new ConstantRNG(-1))._1)
  }

  test("nonNegative should add 1 and invert negative input")
  {
    assert(25 == RNG.nonNegativeInt(new ConstantRNG(-26))._1)
  }

  test("double should return 0.0 as lowest value")
  {
    assert(0.0 == RNG.double(new ConstantRNG(0))._1)
  }

  test("double should return < 1.0 as the highest value")
  {
    assert(RNG.double(new ConstantRNG(Integer.MAX_VALUE))._1 < 1.0)
  }

  test("intDouble should return int and double")
  {
    val (id, rng) = RNG.intDouble(new ConstantRNG(-1))
    assert((-1, 0.0) == id)
  }

  test("doubleInt should return double and int")
  {
    val (di, rng) = RNG.doubleInt(new ConstantRNG(-1))
    assert((0.0, -1) == di)
  }

  test("double3 should return 3 doubles")
  {
    val (ds, rng) = RNG.double3(new ConstantRNG(-1))
    assert((0.0, 0.0, 0.0) == ds)
  }

  test("ints should return list of random ints")
  {
    val rng = new ConstantRNG(-1)
    assert(Nil == RNG.ints(0)(rng)._1)
    assert(List(-1) == RNG.ints(1)(rng)._1)
    assert(List(-1, -1, -1) == RNG.ints(3)(rng)._1)
  }
}
