
class Index(
  val row: Int, 
  val col: Int,
  val value: Char
) {

  override def toString: String = {
    "{"+row+" "+col+"}"
  }
}

abstract class Line(
  val origin: Index, 
  val puzzle: Puzzle
) {

  def getMatchIndexes(keyword: String): List[Index] = {
    def checkGenerateIndex(i: Int, idxList: List[Index]): List[Index] = {
      if (i == keyword.length)
        idxList.reverse
      else if (keyword.charAt(i) == idxList.head.value)
        checkGenerateIndex(i+1, getNextIndex(idxList.head) +: idxList)
      else
        Nil
    }

    checkGenerateIndex(0, origin +: Nil)
  }

  def getNextIndex(idx: Index): Index
}

class UpLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, value, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.above(idx)
  }
}

class UpRightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, value, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.aboveRight(idx)
  }
}

class RightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, value, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.right(idx)
  }
}

class DownRightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, value, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.belowRight(idx)
  }
}

class DownLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, value, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.below(idx)
  }
}

class Puzzle(c: String, s: Int) {
  val contents: String = c
  val width: Int = s
  val height: Int = c.length / s

  def above(idx: Index): Index = {
    if (idx.row > 0)
      new Index(idx.row-1, idx.col, contents.charAt(idx.row*width+idx.col))
    else
      null
  }

  def aboveRight(idx: Index): Index = {
    if (idx.col < width && idx.row > 0)
      new Index(idx.row-1, idx.col+1, contents.charAt(idx.row*width+idx.col))
    else
      null
  }

  def right(idx: Index): Index = {
    if (idx.col < width)
      new Index(idx.row, idx.col+1, contents.charAt(idx.row*width+idx.col))
    else
      null
  }

  def belowRight(idx: Index): Index = {
    if (idx.col < width && idx.row < height)
      new Index(idx.row+1, idx.col+1, contents.charAt(idx.row*width+idx.col))
    else
      null
  }

  def below(idx: Index): Index = {
    if (idx.row < height)
      new Index(idx.row+1, idx.col, contents.charAt(idx.row*width+idx.col))
    else
      null
  }

  def linesForIndexOf(c: Char, idx: Index): List[Line] = {
     val flatIdx = idx.row*width + idx.col + 1
     val i = contents.indexOf(c, flatIdx)
     if (i == -1)
       Nil
     else {
       val origin = new Index(i/width, i%width, contents.charAt(i))
       List(
         UpLine(origin, this), 
         UpRightLine(origin, this), 
         RightLine(origin, this), 
         DownRightLine(origin, this),
         DownLine(origin, this)
       )
     }
  }

  def find(keyword: String): List[Index] = {
    def findTR(lines: List[Line]) {
      if (lines == Nil) {
        Nil
      }
      else {
        
      }
    }
  }

//  def getLinesForIndex(idx: Index): List[Line] = {
//  }
}

//val idx = new Index(41, 10)
//val vline = new VertLine(idx, "asdf")
//println(idx)
//println(vline.getMatchIndexes("asd"))
//println(vline.getMatchIndexes("asf"))
//
//val hline = new HorizLine(idx, "asdf")
//println(hline.getMatchIndexes("asdf"))
//println(hline.getMatchIndexes("asf"))
