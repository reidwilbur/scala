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

  test("3 Stream.takeWhile returns correct value") {
    assert(Stream.empty == Stream().takeWhile(_ => true))
    assert(Stream.empty == Stream().takeWhile(_ => false))
    assert(List(1,2,3) == Stream(1,2,3).takeWhile(_ => true).toList)
    assert(List(1,2) == Stream(1,2,3).takeWhile(_ < 3).toList)
    assert(Nil == Stream(1,2,3).takeWhile(_ => false).toList)
  }

  test("4 Stream.forAll returns correct value") {
    assert(true == Stream[Int]().forAll(_ > 0))
    assert(true == Stream[Int](1).forAll(_ > 0))
    assert(false == Stream[Int](0).forAll(_ > 0))
    assert(false== Stream[Int](1,2,3,0).forAll(_ > 0))
    assert(false== Stream[Int](0,1,2,3).forAll(_ > 0))
  }

  test("6 Stream.headOption returns correct value") {
    assert(None == Stream().headOption)
    assert(Some(1) == Stream(1).headOption)
    assert(Some(1) == Stream(1,2,3).headOption)
  }

  test("7 Stream.map returns correct value") {
    assert(Nil == Stream().map(_.toString).toList)
    assert(List("1") == Stream(1).map(_.toString).toList)
    assert(List("1", "2", "3") == Stream(1,2,3).map(_.toString).toList)
  }

  test("7 Stream.filter returns correct value") {
    assert(Nil == Stream[Int]().filter(_ > 0).toList)
    assert(List(1) == Stream(1).filter(_ < 3).toList)
    assert(List(1,2,3) == Stream(1,2,3,4).filter(_ < 4).toList)
  }

  test("7 Stream.append returns correct value") {
    assert(Nil == Stream[Int]().append(Stream[Int]()).toList)
    assert(List(1) == Stream[Int]().append(Stream(1)).toList)
    assert(List(2,3,4,1) == Stream[Int](2,3,4).append(Stream(1)).toList)
    assert(List(2,3,4,5,6,7) == Stream[Int](2,3,4).append(Stream(5,6,7)).toList)
  }

  test("7 Stream.flatMap returns correct value") {
    assert(Nil == Stream().flatMap{_ => Stream()}.toList)
    assert(Nil == Stream[Int]().flatMap{_ => Stream(1)}.toList)
    assert(List(2,2,2) == Stream(1).flatMap{ i => Stream(i+1, i+1, i+1) }.toList)
    assert(List(2,2,3,3,4,4) == Stream(1, 2, 3).flatMap{ i => Stream(i+1, i+1) }.toList)
  }

  test("8 Stream.constant return correct value") {
    assert(List("foo", "foo", "foo") == Stream.constant("foo").take(3).toList)
  }
}
