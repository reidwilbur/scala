package fpinscala.state

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class StateSuite extends FunSuite {
  test("1 RNG.nonNegativeInt returns correct value") {
    val (nni, _) = RNG.nonNegativeInt(RNG.Simple(42))
    assert(nni > 0)
  }

  test("2 RNG.double returns correct value") {
    val (d, _) = RNG.double(RNG.Simple(42))
    assert(d >= 0.0)
    assert(d < 1.0)
  }

  test("nonNegativeEven returns correct value") {
    val rand = RNG.nonNegativeEven(RNG.Simple(42))
    val v = rand._1
    assert(v % 2 == 0)
    assert(v > 0)
  }

  test("5 RNG.double returns correct value") {
    val rand = RNG.double(RNG.Simple(42))
    val v = rand._1
    assert(v > 0.0)
    assert(v < 1.0)
  }

  test("6 RNG.map2 returns correct value") {
    val id_rnd = RNG.map2(RNG.nonNegativeInt, RNG.double)((i,d) => (i,d))(RNG.Simple(42))
    assert(id_rnd._1._1 > 0)
    assert(id_rnd._1._2 >= 0.0)
    assert(id_rnd._1._2 < 1.0)
  }
}
