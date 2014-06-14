package fpinscala.errorhandling

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class OptionSuite extends FunSuite {
  test("1 OOption.map returns correct value") {
    
    assert(SSome(2) == SSome(1).map(i => i+1))

    assert(NNone == NNone.map(_.toString))
  }

  test("1 OOption.flatMap returns correct value") {

    def plusOneIfLtThree(i: Int): OOption[Int] = {
      if (i < 3) SSome(i+1) else NNone
    }

    assert(SSome(2) == SSome(1).flatMap(plusOneIfLtThree))
    assert(NNone == SSome(3).flatMap(plusOneIfLtThree))
    assert(NNone == NNone.flatMap(plusOneIfLtThree))
  }

  test("1 OOption.getOrElse returns correct value") {

    assert(2 == SSome(2).getOrElse(0))
    assert(0 == NNone.getOrElse(0))
  }

  test("1 OOption.orElse returns correct value") {

    assert(SSome(2) == SSome(2).orElse(SSome(3)))
    assert(SSome(3) == NNone.orElse(SSome(3)))
    assert(NNone == NNone.orElse(NNone))
  }

  test("1 OOption.filter returns correct value") {

    def fltr(i: Int): Boolean = i < 3

    assert(SSome(2) == SSome(2).filter(fltr))
    assert(NNone == SSome(3).filter(fltr))
    assert(NNone == NNone.filter(fltr))
  }

  test("2 OOption.variance returns correct value") {
    assert(NNone == OOption.variance(Seq[Double]()))

    assert(SSome(0.0) == OOption.variance(Seq(1.0, 1.0, 1.0, 1.0)))

    assert(SSome(0.25) == OOption.variance(Seq(1.0, 2.0)))
  }

  test("3 OOption.map2 returns correct value") {
    def f(i: Int, s: Double): String = 
      (i + s).toString

    assert(SSome("3.0") == OOption.map2(SSome(1), SSome(2.0))(f))
    assert(NNone == OOption.map2(NNone, SSome(2.0))(f))
    assert(NNone == OOption.map2(SSome(1), NNone)(f))
  }

  test("4 OOption.sequence returns correct value") {
    assert(SSome(List(1,2,3)) == OOption.sequence(List(SSome(1), SSome(2), SSome(3))))
    assert(SSome(Nil) == OOption.sequence(List[OOption[Int]]()))
    assert(NNone == OOption.sequence(List(SSome(1), SSome(2), NNone)))
    assert(NNone == OOption.sequence(List(SSome(1), NNone, SSome(2))))
    assert(NNone == OOption.sequence(List(NNone, SSome(1), SSome(2))))
    assert(NNone == OOption.sequence(List(NNone)))
  }

  test("5 OOption.traverse returns correct value") {
    assert(SSome(List(1,2,3)) == OOption.traverse(List("1","2","3"))(x => OOption.Try(x.toInt)))
    assert(NNone == OOption.traverse(List("1","asfd","3"))(x => OOption.Try(x.toInt)))
  }
}
