
object p23 {
  def randomSelect[T](numEls: Int, list: List[T]): List[T] = {
    if (numEls > list.length) 
      throw new IllegalArgumentException(
        "numEls "+numEls+" greater than length of list "+list.length
        )

    import scala.util.Random
    val rndm = new Random(System.currentTimeMillis)

    def randomSelectTR[T](n: Int, rsel: List[T], list: List[T]): List[T] =
      (n, list) match {
        case (0, _) => rsel
        case (_, l) =>
          val idx = rndm.nextInt(l.length)
          val (remain, el) = p20.removeAt(idx, l)
          randomSelectTR(n-1, el :: rsel, remain)
      }
      
    randomSelectTR(numEls, Nil, list)
  }
}

