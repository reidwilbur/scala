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

  test("8 Stream.constant returns correct value") {
    assert(List("foo", "foo", "foo") == Stream.constant("foo").take(3).toList)
  }

  test("9 Stream.from returns correct value") {
    val from10 = Stream.from(10)

    assert(List(10, 11, 12) == from10.take(3).toList)
    assert(List(11, 13) == from10.take(5).filter(_ % 2 == 1).toList)
  }

  test("10 Stream.fibs returns correct value") {
    assert(List(0, 1, 1, 2, 3, 5, 8, 13, 21) == Stream.fibs.take(9).toList)
  }

  test("11 Stream.unfold returns correct value") {
    assert(List(1,2,3,4,5) == Stream.unfold(1)( i => Some(i,i+1) ).take(5).toList)
  }

  test("13 Stream.zipWith returns correct value") {
    val s1 = Stream(1,2,3)
    val s2 = Stream("foo", "bar", "baz")

    assert(List((1,"foo"),(2,"bar"),(3,"baz")) == s1.zipWith(s2)((_,_)).toList)
    assert(Nil == Stream().zipWith(s2)((_,_)).toList)
    assert(List((1,"foo")) == Stream(1).zipWith(s2)((_,_)).toList)
  }

  test("13 Stream.zipAll returns correct value") {
    val s1 = Stream(1,2)
    val s2 = Stream("foo", "bar", "baz")
   
    assert(List((Some(1),Some("foo")), (Some(2),Some("bar")), (None, Some("baz")))
           == s1.zipAll(s2).toList)
    
    assert(List((None,Some("foo")), (None,Some("bar")), (None, Some("baz")))
           == Stream().zipAll(s2).toList)
  }

  test("14 Stream.startsWith returns correct value") {
    assert(Stream(1,2,3,4).startsWith(Stream(1,2)))
    assert(Stream(1,2,3,4).startsWith(Stream()))
    assert(!Stream(1,2,3,4).startsWith(Stream(1,2,4)))
  }

  test("15 Stream.tails returns correct value") {
    val ts = Stream(1,2,3).tails.toList
    assert(List(1,2,3) == ts.head.toList)
    assert(List(2,3)   == ts.drop(1).head.toList)
    assert(List(3)     == ts.drop(2).head.toList)
    assert(Nil         == ts.drop(3).head.toList)
  }
}
