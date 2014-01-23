package ch4

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PriorityQueueSuite extends FunSuite {
   test("head of empty q returns none") {
     val queue = new VectorImpl[Int]()
     assert(queue.head == None)
   }
   
   test("head returns inserted element") {
     val queue = new VectorImpl[Int]()
     
     queue.insert(3)
     assert(queue.head == Some(3))
   }
   
   test("insert maintains ordering") {
     val queue = new VectorImpl[Int]()
     
     queue.insert(3)
     assert(queue.head == Some(3))
     
     queue.insert(2)
     assert(queue.head == Some(2))
     
     queue.insert(1)
     assert(queue.head == Some(1))
     
     queue.insert(4)
     assert(queue.head == Some(1))
   }
   
   test("parent calculates indexes correctly") {
     val queue: VectorImpl[Int] = new VectorImpl[Int]
     
     assert(queue.parent(0) == None)
     assert(queue.parent(1) == Some(0))
     assert(queue.parent(2) == Some(0))
     assert(queue.parent(3) == Some(1))
     assert(queue.parent(4) == Some(1))
     assert(queue.parent(5) == Some(2))     
     assert(queue.parent(6) == Some(2))
   }
   
   test("leftChild calculates indexes correctly") {
     val queue: VectorImpl[Int] = new VectorImpl[Int]
     
     assert(queue.leftChild(0) == 1)
     assert(queue.leftChild(1) == 3)
     assert(queue.leftChild(2) == 5)
     assert(queue.leftChild(3) == 7)
     assert(queue.leftChild(4) == 9)
   }
   
   test("rightChild calculates indexes correctly") {
     val queue = new VectorImpl[Int]
     
     assert(queue.rightChild(0) == 2)
     assert(queue.rightChild(1) == 4)
     assert(queue.rightChild(2) == 6)
     assert(queue.rightChild(3) == 8)
     assert(queue.rightChild(4) == 10)
   }
   
   test("remove returns correct element and updates head") {
     val queue = new VectorImpl[Int]
     
     queue.insert(3).insert(2).insert(1).insert(4)
     
     assert(queue.remove == Some(1))
     assert(queue.remove == Some(2))
     assert(queue.remove == Some(3))
     assert(queue.remove == Some(4))
     assert(queue.remove == None)
   }
}