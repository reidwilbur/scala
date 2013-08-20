
def range(start: Int, end: Int): List[Int] = {
  def rangeTR(curr: Int, r: List[Int]): List[Int] =
    curr match {
      case _ if curr == start => start :: r
      case x => rangeTR(curr - 1 , x :: r)
    }

  if (start > end) throw new IllegalArgumentException("Start must be less than end")

  rangeTR(end, Nil)
}

val r = range(4, 9)
println(r)
assert(r == List(4, 5, 6, 7, 8, 9))
