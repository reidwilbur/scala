
class Index(
  val row: Int, 
  val col: Int,
  val value: Char
) {

  override def toString: String = {
    value+" {"+row+" "+col+"}"
  }
}

abstract class Line(
  val origin: Index, 
  val puzzle: Puzzle
) {

  def getMatchIndexes(keyword: String): List[Index] = {
    def checkGenerateIndex(i: Int, idxList: List[Index]): List[Index] = {
      println(i+" "+keyword+" "+idxList)
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

class RightLine(
  origin: Index, 
  puzzle: Puzzle
) extends Line(origin, puzzle) {

  override def getNextIndex(idx: Index): Index = {
    puzzle.right(idx)
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
    if (idx.col < width && idx.row > 0)
      new Index(idx.row-1, idx.col+1, contents.charAt( ((idx.row-1)*width)+idx.col+1))
    else
      null
  }

  def right(idx: Index): Index = {
    if (idx.col < width)
      new Index(idx.row, idx.col+1, contents.charAt(idx.row*width+idx.col+1))
    else
      null
  }

  def belowRight(idx: Index): Index = {
    if (idx.col < width && idx.row < height)
      new Index(idx.row+1, idx.col+1, contents.charAt((idx.row+1)*width+idx.col+1))
    else
      null
  }

  def below(idx: Index): Index = {
    if (idx.row < height)
      new Index(idx.row+1, idx.col, contents.charAt((idx.row+1)*width+idx.col))
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
     else 
       new Index(i/width, i%width, contents.charAt(i))
  }

  def findInIndex(keyword: String, idx: Index): List[Index] = {
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
         new RightLine(idx, this), 
         new DownRightLine(idx, this),
         new DownLine(idx, this)
       )
     )
  }

  def find(keyword: String): List[Index] = {
    def findTR(index: Index): List[Index] = {
      if (index == null)
        Nil
      else {
        val matchedIndexes = findInIndex(keyword, index)
        if (matchedIndexes != Nil)
          matchedIndexes
        else
          findTR(next(index))
      }
    }
    
    findTR(firstIndex)
  }

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

val puzzle = new Puzzle("SCALAALHPCUAMNJWAYDXBOSRVSTRVOYOAWKHUQZPJVUEOSNNPFOKLNTLMCEGULA", 9)

val words = List("SCALA", "HASKEL", "PYTHON", "ML", "JS", "RUST", "AWK", "JAVA", "LUA", "CPP", "GO")
words.foreach(word => println(word+" "+puzzle.find(word)))
