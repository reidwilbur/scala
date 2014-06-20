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
}
