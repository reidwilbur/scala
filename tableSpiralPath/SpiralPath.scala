
case class Pos(val row: Int, val col: Int) {
  def flatten(width: Int, height: Int): Int = (width * row) + col + 1

  def offset(idx: Pos): Pos = Pos(idx.row+row, idx.col+col)

  def isInside(width: Int, height: Int): Boolean = 
    (row >= 0) && (col >= 0) && (row < height) && (col < width)

  override def toString: String = "(Pos:["+row+","+col+"])"
}

def spiralPath(height: Int, width: Int, startRow: Int, startCol: Int): List[Int] = {
  val spiralOffset = new Iterator[Pos] {
    var curSegLen = 0
    var nextOffsets: List[Pos] = Nil
  
    def hasNext = true
  
    def next(): Pos = {
      nextOffsets match {
        case h :: t => 
          nextOffsets = t
          h
        case Nil =>
          curSegLen += 1
          val dir = if (curSegLen % 2 == 0) 1 else -1
          val newOffsets = 
            (for(j <- (0 until curSegLen).toList) yield Pos(dir, 0)) ::: (for(j <- (0 until curSegLen).toList) yield Pos(0, dir))
          nextOffsets = newOffsets.tail
          newOffsets.head
      }
    }
  }

  val finalPos = Pos(0, width-1)

  import scala.annotation.tailrec
  @tailrec
  def walkPath(pos: Pos, path: List[Pos]): List[Pos] = {
    val ofs = spiralOffset.next()
    pos match {
      case p if (p == finalPos) => (p :: path).reverse
      case p if (p.isInside(width, height)) =>
        walkPath(p.offset(ofs), p :: path)
      case p =>
        walkPath(p.offset(ofs), path)
    }
  }

  walkPath(Pos(startRow-1, startCol-1), Nil).map( _.flatten(width, height))
}

assert(
  List(13,8,7,12,17,18,19,14,9,4,3,2,1,6,11,16,21,22,23,24,25,20,15,10,5)
  == spiralPath(5,5,3,3)
)
assert(List(2,1,5,6,7,3,8,4) == spiralPath(2,4,1,2))

