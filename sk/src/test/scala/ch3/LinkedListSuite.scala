package ch3

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LinkedListSuite extends FunSuite {
  import LinkedList._
  
  test("case1: reverse single node list") {
    val n = new Node[Int](1)
    
    val r = LinkedList.reverseList(n)
    assert(r.value == 1)
    assert(r.next == None)
  }
  
  test("case2: reverse double node list") {
    val n2 = new Node[Int](2)
    val n1 = new Node[Int](1, n2)
    
    val r = LinkedList.reverseList(n1)
    assert(r.value == 2)
    assert(r.next.get.value == 1)
    assert(r.next.get.next == None)
  }

  test("case2: reverse multi node list") {
    val n3 = new Node[Int](3)
    val n2 = new Node[Int](2, n3)
    val n1 = new Node[Int](1 ,n2)
    
    val r = LinkedList.reverseList(n1)
    assert(r.value == 3)
    assert(r.next.get.value == 2)
    assert(r.next.get.next.get.value == 1)
    assert(r.next.get.next.get.next == None)
  }
}