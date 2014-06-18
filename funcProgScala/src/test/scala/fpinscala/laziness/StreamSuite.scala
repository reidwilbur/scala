package fpinscala.laziness

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class StreamSuite extends FunSuite {
  test("1 Stream.toList returns correct value") {
    val stream = Stream(1, 2, 3, 4, 5)

    assert(List(1,2,3,4,5) == stream.toList)
  }

  test("2 Stream.take returns correct value") {
    assert(Stream.empty == Stream().take(0))
    assert(Stream.empty == Stream().take(1))
    assert(Stream.empty == Stream(1, 2, 3).take(0))
    assert(List(1) == Stream(1,2,3).take(1).toList)
    assert(List(1,2,3) == Stream(1,2,3).take(3).toList)
    assert(List(1,2,3) == Stream(1,2,3).take(4).toList)
  }

  test("2 Stream.drop returns correct value") {
    assert(Stream.empty == Stream().drop(0))
    assert(Stream.empty == Stream().drop(1))
    assert(List(1,2,3) == Stream(1, 2, 3).drop(0).toList)
    assert(List(2,3) == Stream(1,2,3).drop(1).toList)
    assert(Nil == Stream(1,2,3).drop(3).toList)
    assert(Nil == Stream(1,2,3).drop(4).toList)
  }
}
