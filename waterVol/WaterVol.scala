
def waterVol(heights: List[Int]): Int = {
  def getLocalMax(heights: List[Int], curMax: Int, maxHeights: List[Int]): List[Int] = {
    heights match {
      case Nil => maxHeights.reverse
      case h :: t => 
        val nextMax = if (h > curMax) h else curMax
        getLocalMax(t, nextMax, (nextMax :: maxHeights))
    }
  }

  val lMax = getLocalMax(heights, 0, Nil)
  val rMax = getLocalMax(heights.reverse, 0, Nil).reverse

  val maxLvl = lMax.zip(rMax).map{ case (l,r) => math.min(l,r) }

  val waterHeights = maxLvl.zip(heights).map{ case (m, h) => m - h }
  waterHeights.foldLeft(0)( (vol, h) => vol+h )
}

assert(waterVol(List(2, 5, 1, 2, 3, 4, 7, 7, 6)) == 10)
assert(waterVol(List(2, 5, 1, 3, 1, 2, 1, 7, 7, 6)) == 17)
assert(waterVol(List(2, 3, 1, 2, 3, 1, 3)) == 5)
assert(waterVol(List(1, 2, 3, 4, 5, 6, 7, 8, 9)) == 0)
assert(waterVol(List(9, 8, 7, 6, 5, 4, 3, 2, 1)) == 0)
assert(waterVol(List(1, 1, 1, 1, 1)) == 0)
assert(waterVol(List(1, 0, 1)) == 1)
assert(waterVol(List(5, 0, 5)) == 5)
assert(waterVol(List(5, 0, 4)) == 4)
assert(waterVol(List(4, 0, 5)) == 4)
assert(waterVol(List(4, 0, 5, 0, 2)) == 6)
assert(waterVol(List(0, 1, 0, 1, 0)) == 1)
assert(waterVol(List(0, 1, 0, 0, 1, 0)) == 2)
assert(waterVol(List(4, 2, 2, 1, 1, 1, 3)) == 8)
assert(waterVol(List(0, 3, 2, 1, 4)) == 3)
assert(waterVol(List(1, 0, 1, 0)) == 1)
assert(waterVol(List(1, 0, 1, 2, 0, 2)) == 3)
assert(waterVol(List(2, 5, 1, 2, 3, 4, 7, 7, 6)) == 10)
assert(waterVol(List(5, 1, 0, 1)) == 1)
assert(waterVol(List(2, 5, 1, 2, 3, 4, 7, 7, 6, 3, 5)) == 12)
assert(waterVol(List(3, 0, 1, 0, 2)) == 5)

