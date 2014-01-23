package rover

case class Coordinate(x: Int, y: Int)

sealed trait Size {
  def x: Int
  def y: Int
  def wrapX(i: Int): Int
  def wrapY(i: Int): Int
}

case class FiniteSize(x: Int, y: Int) extends Size {
  def wrap(i: Int, max: Int): Int = {
    val m = i % max
    if (m < 0) max + m else m
  }
  override def wrapX(i: Int): Int = wrap(i, x)
  override def wrapY(i: Int): Int = wrap(i, y)
}

case object InfiniteSize extends Size {
  override def x = -1
  override def y = -1
  override def wrapX(i: Int) = i
  override def wrapY(i: Int) = i
}

object RoverImplicits {
  implicit def tupleToCoord(tuple: Tuple2[Int,Int]): Coordinate = {
    Coordinate(tuple._1, tuple._2)
  }
  
  implicit def tupleToSize(tuple: Tuple2[Int,Int]): Size ={
    FiniteSize(tuple._1, tuple._2)
  }
}

