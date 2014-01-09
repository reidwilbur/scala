package ch3

object BinPack {
  import scala.collection.immutable.TreeSet
  
  case class Bin(weight: Double) {
    assert(weight <= 1.0)
    
    def weightLeft: Double = 1.0 - weight
  }
  
  def packBestFit(weights: List[Double]): Int = {
    implicit def binOrdering: Ordering[Bin] = Ordering.by[Bin,Double]{_.weightLeft}
    pack(weights)
  }
  
  def packWorstFit(weights: List[Double]): Int = {
    implicit def binOrdering: Ordering[Bin] = Ordering.by[Bin,Double]{_.weightLeft}.reverse
    pack(weights)
  }
  
  def pack(weights: List[Double])(implicit ordering: Ordering[Bin]): Int = {
    //outer loop iteration n times
    val bins = weights.foldLeft(TreeSet[Bin]())
	    { (bins, weight) =>
	      //O(log n) to create in order iterator
	      val possibleBins = bins.dropWhile{_.weightLeft < weight}
	      possibleBins.headOption match {
	        case Some(bin) =>
	          val newBin = Bin(bin.weight + weight)
	          //O(log n) + O(log n)
	          (bins - bin) + newBin
	        case None =>
	          //O(log n)
	          bins + Bin(weight)
	      }
	    }
    //overall O(n (log n + 2log n)) = O(n log n) 
    
    bins.size    
  }
}
