package fpinscala.errorhandling

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class EitherSuite extends FunSuite {
  test("6 EEither.map returns correct value") {
    assert(RRight(2) == RRight(1).map(i => i+1))
    assert(LLeft("foo") == LLeft("foo").map(s => "bar"))
  }

  test("6 EEither.flatMap returns correct value") {
    def f(i: Int): EEither[String, Int] =
      if (i < 3) RRight(i+1) else LLeft("boof")

    assert(RRight(2)     ==    RRight(1).flatMap(f))
    assert(LLeft("boof") ==    RRight(3).flatMap(f))
    assert(LLeft("foo")  == LLeft("foo").flatMap(f))
  }

  test("6 EEither.orElse returns correct value") {
    assert(RRight(2) == RRight(2).orElse(RRight(3)))
    assert(RRight(3) == LLeft("foo").orElse(RRight(3)))
    assert(LLeft("boof") == LLeft("foo").orElse(LLeft("boof")))
  }

  test("6 EEither.map2 returns correct value") {
    def f(i: Int, d: Double): String = 
      (i + d).toString

    assert(RRight("3.0") == RRight(1).map2(RRight(2.0))(f))
    assert(LLeft("boof") == RRight(1).map2(LLeft("boof"))(f))
    assert(LLeft("boof") == LLeft("boof").map2(RRight(2.0))(f))
  }
}
