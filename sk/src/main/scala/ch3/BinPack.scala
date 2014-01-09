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
    val bins = weights.foldLeft(TreeSet[Bin]())
	    { (bins, weight) =>
	      val possibleBins = bins.dropWhile{_.weightLeft < weight}
	      possibleBins.headOption match {
	        case Some(bin) =>
	          val newBin = Bin(bin.weight + weight)
	          (bins - bin) + newBin
	        case None =>
	          bins + Bin(weight)
	      }
	    }
    
    bins.size    
  }
}