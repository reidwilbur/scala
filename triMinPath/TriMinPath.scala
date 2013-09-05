
def getMinPath(triWeights: List[List[Int]]): Int = {
  def collapseRowsTakeShortest(top: List[Int], base: List[Int]): List[Int] = {
    val (b, collapsed) = 
      top.foldLeft((base, List[Int]()))
        {
          case((b, c), t) =>
            val pl = t + b.head
            val pr = t + b.tail.head
            val ps = if (pl > pr) pr else pl
            (b.tail, ps :: c)
        }
    collapsed.reverse
  }

  def _getMinPath(weights: List[List[Int]]): Int = {
    weights match {
      case xs :: Nil =>
        xs.head
      case base :: top :: rest =>
        val collapsed = collapseRowsTakeShortest(top, base)
        _getMinPath(collapsed :: rest)
    }
  }

  _getMinPath(triWeights.reverse)
}

val path1 = getMinPath(List(
                       List(1),
                      List(2,4),
                     List(5,1,4),
                    List(2,3,4,5)))

assert(path1 == 7)


val path2 = getMinPath(List(
                       List(3),
                      List(2,4),
                     List(1,9,3),
                    List(9,9,2,4),
                   List(4,6,6,7,8),
                  List(5,7,3,5,1,4)))

assert(path2 == 20)

val path3 = getMinPath(List(
                       List(3)))

assert(path3 == 3)

val path4 = getMinPath(List(
                       List(3),
                      List(1,2)))

assert(path4 == 4)

