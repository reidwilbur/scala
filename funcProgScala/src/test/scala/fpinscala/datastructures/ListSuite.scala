package fpinscala.datastructures

import org.scalatest.{FunSuite}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {
  test("LList.tail returns the tail of the list") {
    val l = LList(1, 2, 3, 4);

    assert(LList(2, 3, 4) == LList.tail(l))
  }

  test("LList.tail returns NNil for empty list") {
    val l = LList();

    assert(NNil == LList.tail(l))
  }

  test("LList.setHead returns list with updated head") {
    val l1 = LList(1, 2, 3)

    val l2 = LList.setHead(33, l1)

    assert(LList(33, 2, 3) == l2)

    val l3 = LList.setHead(33, NNil)

    assert(LList(33) == l3)
  }

  test("LList.drop drops first n items from list") {
    val l1 = LList(1, 2, 3, 4, 5, 6)

    val l2 = LList.drop(l1, 2)

    assert(LList(3, 4, 5, 6) == l2)

    assert(NNil == LList.drop(l1, 35))

    assert(l1 == LList.drop(l1, 0))
  }

  test("LList.dropWhile drops first items while condition is true") {
    val l1 = LList(1, 2, 3, 4, 5, 6)

    val l2 = LList.dropWhile(l1){ _ < 4 }

    assert(LList(4, 5, 6) == l2)

    assert(NNil == LList.dropWhile(l1){ _ != 100 })

    assert(l1 == LList.dropWhile(l1){ _ < 0 })
  }

  test("LList.init returns all but last element") {
    val l1 = LList(1, 2, 3, 4, 5)

    assert(LList(1,2,3,4) == LList.init(l1))

    assert(NNil == LList.init(LList(1)))

    assert(NNil == LList.init(NNil))
  }

  test("foldright nil and cons") {
    println(LList.foldRight(LList(1,2,3), NNil:LList[Int])(CCons(_, _)))
  }

  test("LList.length returns length of list") {
    val l1 = LList(1,2,3,4)

    assert(4 == LList.length(l1))

    assert(0 == LList.length(NNil))
  }

  test("LList.foldLeft returns correct value") {
    val l1 = LList(1,2,3,4)

    assert(10 == LList.foldLeft(l1, 0)( (z, x) => z + x ) )
  }

  test("LList.sumFoldLeft returns correct value") {
    val l1 = LList(1,2,3,4)

    assert(10 == LList.sumFoldLeft(l1))
    
    assert(0 == LList.sumFoldLeft(NNil))
  }

  test("LList.prodFoldLeft returns correct value") {
    val l1 = LList(1.0, 2.0, 3.0, 4.0)

    assert(24.0 == LList.prodFoldLeft(l1))
    
    assert(1.0 == LList.prodFoldLeft(NNil))
  }

  test("LList.lengthFoldLeft returns correct value") {
    val l1 = LList(1.0, 2.0, 3.0, 4.0)

    assert(4 == LList.lengthFoldLeft(l1))
    
    assert(0 == LList.lengthFoldLeft(NNil))
  }

  test("LList.reverse returns correct value") {
    val l1 = LList(1, 2, 3, 4)

    assert(LList(4, 3, 2, 1) == LList.reverse(l1))
    
    assert(NNil == LList.reverse(NNil))
  }

  test("LList.appendWithFoldRight returns correct value") {
    val l1 = LList(1, 2, 3, 4)

    assert(LList(1,2,3,4,0) == LList.appendWithFoldRight(l1, LList(0)))
    
  }
  
  test("LList.foldRightWithFoldLeft implements fold right") {
    val l1 = LList(1,2,3,4)

    val s1 = LList.foldRightWithFoldLeft(l1, 0){ (x, z) => z - x }
    val s2 = LList.foldRight(l1, 0){ (x, z) => z - x}

    assert(s1 == s2)
  }
  
  test("LList.coalesce returns correct value") {
    val ll = LList(LList(1,2,3), LList(3,4,5), LList(0))

    val lc = LList.coalesce(ll)

    assert(LList(1,2,3,3,4,5,0) == lc)
  }
  
  test("LList.add1 returns correct value") {
    val l = LList(1,2,3,4)

    assert(LList(2,3,4,5) == LList.add1(l))
    assert(LList(1,2,3,4) == l)
  }
  
  test("LList.doublesToString returns correct value") {
    val l = LList(1.0, 2.0, 3.0, 4.0)

    assert(LList("1.0", "2.0", "3.0", "4.0") == LList.doublesToStrings(l))
  }
  
  test("LList.map returns correct value") {
    val l = LList(1.0, 2.0, 3.0, 4.0)

    assert(LList("1.0", "2.0", "3.0", "4.0") == LList.map(l)(_.toString))
  }
  
  test("LList.filter returns correct value") {
    val l = LList(1, 2, 3, 4, 6, 7, 9, 10)

    assert(LList(2,4,6,10) == LList.filter(l)(_ % 2 == 0))
  }
  
  test("LList.flatMap returns correct value") {
    val l = LList(1, 2, 3, 4)

    assert(LList(1,1,2,2,3,3,4,4) == LList.flatMap(l)(x => LList(x,x)))
  }
  
  test("LList.filterViaFlatMap returns correct value") {
    val l = LList(1, 2, 3, 4, 6, 7, 9, 10)

    assert(LList(2,4,6,10) == LList.filterViaFlatMap(l)(_ % 2 == 0))
  }

  test("LList.sumLists returns correct value") {
    val l1 = LList(1, 2, 3)
    val l2 = LList(4, 5, 6)

    assert(LList(5,7,9) == LList.sumLists(l1,l2))

    val l3 = LList(4,5,6,7,8,9)

    assert(LList(5,7,9,7,8,9) == LList.sumLists(l1,l3))
  }

  test("LList.pairwise returns correct value") {
    val l1 = LList(1, 2, 3)
    val l2 = LList(4, 5, 6)

    assert(LList(5,7,9) == LList.pairwise(l1,l2)((l,r) => l + r))

    val l3 = LList(4,5,6,7,8,9)

    assert(LList(5,7,9) == LList.pairwise(l1,l3)((l,r) => l + r))
  }

  test("LList.hasSubseq returns correct value") {
    val l1 = LList(1,2,3,4,5,6)

    assert(true == LList.hasSubseq(l1, NNil))

    assert(true == LList.hasSubseq(l1, LList(1)))
    assert(true == LList.hasSubseq(l1, LList(6)))

    assert(false == LList.hasSubseq(l1, LList(-1)))
    assert(false == LList.hasSubseq(l1, LList(7)))

    assert(true == LList.hasSubseq(l1, LList(1,2,3)))
    assert(true == LList.hasSubseq(l1, LList(4,5,6)))
    assert(true == LList.hasSubseq(l1, LList(3,4,5)))

    assert(false == LList.hasSubseq(l1, LList(1,2,4)))
    assert(false == LList.hasSubseq(l1, LList(4,5,7)))
    assert(false == LList.hasSubseq(l1, LList(3,4,6)))
  }
}
