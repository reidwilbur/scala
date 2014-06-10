package fpinscala.datastructures

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {
  test("List.tail returns the tail of the list") {
    val l = fpinscala.datastructures.List(1, 2, 3, 4);

    assert(fpinscala.datastructures.List(2, 3, 4) == fpinscala.datastructures.List.tail(l))
  }

  test("List.tail returns Nil for empty list") {
    val l = fpinscala.datastructures.List();

    assert(fpinscala.datastructures.Nil == fpinscala.datastructures.List.tail(l))
  }

  test("List.setHead returns list with updated head") {
    val l1 = fpinscala.datastructures.List(1, 2, 3)

    val l2 = fpinscala.datastructures.List.setHead(33, l1)

    assert(fpinscala.datastructures.List(33, 2, 3) == l2)

    val l3 = fpinscala.datastructures.List.setHead(33, fpinscala.datastructures.Nil)

    assert(fpinscala.datastructures.List(33) == l3)
  }

  test("List.drop drops first n items from list") {
    val l1 = List(1, 2, 3, 4, 5, 6)

    val l2 = List.drop(l1, 2)

    assert(List(3, 4, 5, 6) == l2)

    assert(Nil == List.drop(l1, 35))

    assert(l1 == List.drop(l1, 0))
  }

  test("List.dropWhile drops first items while condition is true") {
    val l1 = List(1, 2, 3, 4, 5, 6)

    val l2 = List.dropWhile(l1){ _ < 4 }

    assert(List(4, 5, 6) == l2)

    assert(Nil == List.dropWhile(l1){ _ != 100 })

    assert(l1 == List.dropWhile(l1){ _ < 0 })
  }

  test("List.init returns all but last element") {
    val l1 = List(1, 2, 3, 4, 5)

    assert(List(1,2,3,4) == List.init(l1))

    assert(Nil == List.init(List(1)))

    assert(Nil == List.init(Nil))
  }

  test("foldright nil and cons") {
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))
  }

  test("List.length returns length of list") {
    val l1 = List(1,2,3,4)

    assert(4 == List.length(l1))

    assert(0 == List.length(Nil))
  }

  test("List.foldLeft returns correct value") {
    val l1 = List(1,2,3,4)

    assert(10 == List.foldLeft(l1, 0)( (z, x) => z + x ) )
  }

  test("List.sumFoldLeft returns correct value") {
    val l1 = List(1,2,3,4)

    assert(10 == List.sumFoldLeft(l1))
    
    assert(0 == List.sumFoldLeft(Nil))
  }

  test("List.prodFoldLeft returns correct value") {
    val l1 = List(1.0, 2.0, 3.0, 4.0)

    assert(24.0 == List.prodFoldLeft(l1))
    
    assert(1.0 == List.prodFoldLeft(Nil))
  }

  test("List.lengthFoldLeft returns correct value") {
    val l1 = List(1.0, 2.0, 3.0, 4.0)

    assert(4 == List.lengthFoldLeft(l1))
    
    assert(0 == List.lengthFoldLeft(Nil))
  }

  test("List.reverse returns correct value") {
    val l1 = List(1, 2, 3, 4)

    assert(List(4, 3, 2, 1) == List.reverse(l1))
    
    assert(Nil == List.reverse(Nil))
  }
}
