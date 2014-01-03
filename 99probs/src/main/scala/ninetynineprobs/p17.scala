
object p17 {
  def split[T](n: Int, list: List[T]): (List[T],List[T]) = {
    def splitTR[T](r: Int, left: List[T], right: List[T]): (List[T],List[T]) =
      (r, right) match {
        case (_, List()) => 
          (left.reverse ,right)
        case (0, _) => 
          (left.reverse, right)
        case (_, x :: xs) => 
          splitTR(r - 1, x :: left, xs)
      }
  
    splitTR(n, Nil, list)
  }
}

