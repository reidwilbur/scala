
class Index(
  val row: Int, 
  val col: Int,
  val value: Char
) {

  override def toString: String = {
    "{"+row+" "+col+"}"
  }

  def debugString: String = {
    value+" {"+row+" "+col+"}"
  }
}

abstract class Line(
  val origin: Index, 
  val puzzle: Puzzle
) {

  def getMatchIndexes(keyword: String): List[Index] = {
    def checkGenerateIndex(i: Int, idxList: List[Index]): List[Index] = {
      //println(i+" "+keyword+" ("+idxList.map(_.debugString).mkString(" ")+")")
      if (i == keyword.length)
        idxList.reverse.init
      else if (idxList.head == null)
        Nil
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
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.above(idx)
  }
}

class UpRightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.aboveRight(idx)
  }
}

class UpLeftLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.aboveLeft(idx)
  }
}

class RightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.right(idx)
  }
}

class LeftLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.left(idx)
  }
}

class DownRightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.belowRight(idx)
  }
}

class DownLeftLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.belowLeft(idx)
  }
}

class DownLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

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
      new Index(idx.row-1, idx.col, contents.charAt( ((idx.row-1)*width)+idx.col) )
    else
      null
  }

  def aboveRight(idx: Index): Index = {
    if (idx.col < width-1 && idx.row > 0)
      new Index(idx.row-1, idx.col+1, contents.charAt( ((idx.row-1)*width)+idx.col+1))
    else
      null
  }

  def aboveLeft(idx: Index): Index = {
    if (idx.col > 0 && idx.row > 0)
      new Index(idx.row-1, idx.col-1, contents.charAt( ((idx.row-1)*width)+idx.col-1))
    else
      null
  }

  def right(idx: Index): Index = {
    if (idx.col < width-1)
      new Index(idx.row, idx.col+1, contents.charAt(idx.row*width+idx.col+1))
    else
      null
  }

  def left(idx: Index): Index = {
    if (idx.col > 0)
      new Index(idx.row, idx.col-1, contents.charAt(idx.row*width+idx.col+1))
    else
      null
  }

  def belowRight(idx: Index): Index = {
    if (idx.col < width-1 && idx.row < height-1)
      new Index(idx.row+1, idx.col+1, contents.charAt((idx.row+1)*width+idx.col+1))
    else
      null
  }

  def below(idx: Index): Index = {
    if (idx.row < height-1)
      new Index(idx.row+1, idx.col, contents.charAt((idx.row+1)*width+idx.col))
    else
      null
  }

  def belowLeft(idx: Index): Index = {
    if (idx.col > 0 && idx.row < height-1)
      new Index(idx.row+1, idx.col-1, contents.charAt((idx.row+1)*width+idx.col-1))
    else
      null
  }

  def next(idx: Index): Index = {
    val flatIdx = (idx.row * width) + idx.col + 1
    new Index(flatIdx / width, flatIdx % width, contents.charAt(flatIdx))
  }

  def firstIndex: Index = {
    new Index(0,0,contents.charAt(0))
  }

  def nextIndexOf(c: Char, idx: Index): Index = {
     val flatIdx = idx.row*width + idx.col
     val i = contents.indexOf(c, flatIdx)
     if (i == -1)
       null
     else {
       //println("Found "+c+" at "+i)
       new Index(i/width, i%width, contents.charAt(i))
     }
  }

  def findAtIndex(keyword: String, idx: Index): List[Index] = {
     def findTR(lines: List[Line]): List[Index] = {
       if (lines == Nil)
         Nil
       else {
         val idxs = lines.head.getMatchIndexes(keyword)
         if (idxs != Nil)
           idxs
         else
           findTR(lines.tail)
       }
     }

     findTR(
       List(
         new UpLine(idx, this), 
         new UpRightLine(idx, this), 
         new UpLeftLine(idx, this), 
         new RightLine(idx, this), 
         new LeftLine(idx, this), 
         new DownRightLine(idx, this),
         new DownLeftLine(idx, this),
         new DownLine(idx, this)
       )
     )
  }

  def find(keyword: String): List[Index] = {
    def findTR(index: Index): List[Index] = {
      if (index == null)
        Nil
      else {
        val matchedIndexes = findAtIndex(keyword, index)
        if (matchedIndexes != Nil)
          matchedIndexes
        else
          findTR(nextIndexOf(keyword.charAt(0), next(index)))
      }
    }
    
    findTR(nextIndexOf(keyword.charAt(0), firstIndex))
  }

  override def toString: String = {
    def getLines(lineIdx: Int, lines: List[String]): List[String] = {
      if (lineIdx < height) {
        val line = "["+contents.substring(lineIdx*width, (lineIdx+1)*width).mkString(" ")+"]"
        getLines(lineIdx+1, line +: lines)
      }
      else
        lines.reverse
    }

    getLines(0, Nil).mkString("\n")
  }
}

val puzzle = new Puzzle("SCALAALHPCUAMNJWAYDXBOSRVSTRVOYOAWKHUQZPJVUEOSNNPFOKLNTLMCEGULA", 9)

val words = List("SCALA", "HASKELL", "PYTHON", "ML", "JS", "RUST", "AWK", "JAVA", "LUA", "CPP", "GO")
println(puzzle)
println
words.foreach(word => println(word+" ["+puzzle.find(word).mkString(" ")+"]"))
