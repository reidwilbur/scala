package fpinscala.gettingstarted

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class GettingStartedSuite extends FunSuite {
  test("format result with abs") {
    assert("The abs of -42 is 42." == CH2.formatResult("abs", -42, CH2.abs))
  }

  test("format result with fib") {
    assert("The fib of 7 is 13." == CH2.formatResult("fib", 7, CH2.fib))
  }

  def gt[T <: Int](a: T, b: T): Boolean = {
    a > b
  }

  test("isSorted with 0 element array") {
    assert(CH2.isSorted(Array[Int](), gt))
  }

  test("isSorted with 1 element array") {
    assert(CH2.isSorted(Array(1), gt))
  }

  test("isSorted with 3 element unsorted array") {
    assert(false == CH2.isSorted(Array(1, 2, 0), gt))
  }

  test("isSorted with 3 element sorted array") {
    assert(CH2.isSorted(Array(1, 2, 4), gt))
  }
}
