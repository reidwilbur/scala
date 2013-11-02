import scala.annotation.tailrec

def waterVol(heights: Vector[Int]): Int = {
  @tailrec
  def calcVol(l: Int, r: Int, curLvl: Int, curVol: Int): Int = {
    if (l == r)
      curVol
    else {
      if (heights(l) < heights(r)) {
        val newLvl = if (heights(l) > curLvl) heights(l) else curLvl
        val newVol = curVol + (newLvl - heights(l))
        calcVol(l+1, r, newLvl, newVol)
      }
      else {
        val newLvl = if (heights(r) > curLvl) heights(r) else curLvl
        val newVol = curVol + (newLvl - heights(r))
        calcVol(l, r-1, newLvl, newVol)
      }
    }
  }

  calcVol(0, heights.length-1, 0, 0)
}

assert(waterVol(Vector(2, 5, 1, 2, 3, 4, 7, 7, 6)) == 10)
assert(waterVol(Vector(2, 5, 1, 3, 1, 2, 1, 7, 7, 6)) == 17)
assert(waterVol(Vector(2, 3, 1, 2, 3, 1, 3)) == 5)
assert(waterVol(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)) == 0)
assert(waterVol(Vector(9, 8, 7, 6, 5, 4, 3, 2, 1)) == 0)
assert(waterVol(Vector(1, 1, 1, 1, 1)) == 0)
assert(waterVol(Vector(1, 0, 1)) == 1)
assert(waterVol(Vector(5, 0, 5)) == 5)
assert(waterVol(Vector(5, 0, 4)) == 4)
assert(waterVol(Vector(4, 0, 5)) == 4)
assert(waterVol(Vector(4, 0, 5, 0, 2)) == 6)
assert(waterVol(Vector(0, 1, 0, 1, 0)) == 1)
assert(waterVol(Vector(0, 1, 0, 0, 1, 0)) == 2)
assert(waterVol(Vector(4, 2, 2, 1, 1, 1, 3)) == 8)
assert(waterVol(Vector(0, 3, 2, 1, 4)) == 3)
assert(waterVol(Vector(1, 0, 1, 0)) == 1)
assert(waterVol(Vector(1, 0, 1, 2, 0, 2)) == 3)
assert(waterVol(Vector(2, 5, 1, 2, 3, 4, 7, 7, 6)) == 10)
assert(waterVol(Vector(5, 1, 0, 1)) == 1)
assert(waterVol(Vector(2, 5, 1, 2, 3, 4, 7, 7, 6, 3, 5)) == 12)
assert(waterVol(Vector(3, 0, 1, 0, 2)) == 5)

