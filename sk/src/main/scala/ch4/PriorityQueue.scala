package ch4

import scala.annotation.tailrec

trait PriorityQueue[T] {
  def insert(el: T): PriorityQueue[T]
  def remove: Option[T]
  def head: Option[T]
}

class VectorImpl[T](implicit val ordering: Ordering[T]) extends PriorityQueue[T] {
  import scala.collection.mutable.ArrayBuffer

  protected var elements: ArrayBuffer[T] = ArrayBuffer.empty[T]
  protected var numEls: Int = 0
  
  override def insert(el: T): PriorityQueue[T] = {
    if (numEls == elements.size) {
      elements += el
    }
    else {
      elements(numEls) = el
    }
    bubbleUp(numEls)
    numEls += 1
    this
  }
  
  private[ch4] def parent(n: Int): Option[Int] = {
    if (n == 0) {
      None
    }
    else {
      Some((n+1)/2-1)
    }
  }
  
  private[ch4] def leftChild(n: Int): Int = {
    //0->(1,2)  1->(3,4)  2->(5,6)
    rightChild(n)-1
  }
  
  private[ch4] def rightChild(n: Int): Int = {
    ((n+1)*2)
  }
  
  @tailrec
  private[ch4] final def bubbleUp(idx: Int): Unit = {
    val pOpt = parent(idx)
    pOpt match {
      case None =>
      case Some(pIdx) =>
        if (ordering.gt(elements(pIdx), elements(idx))) {
          val tmp = elements(pIdx)
          elements(pIdx) = elements(idx)
          elements(idx) = tmp
          bubbleUp(pIdx)
        }
    }
  }
  
  override def head: Option[T] = {
    numEls match {
      case 0 => None
      case _ => Some(elements(0))
    }
  }
  
  override def remove: Option[T] = {
    numEls match {
      case 0 => None
      case _ =>
        None
    }
  }
}