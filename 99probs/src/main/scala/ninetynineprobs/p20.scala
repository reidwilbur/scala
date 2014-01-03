package ninetynineprobs

object p20 {
  def removeAt[T](idx: Int, list: List[T]): (List[T], T) = {
    def removeAtTR[T](i: Int, shifted: List[T], list: List[T]): (List[T], T) = 
      (i, list) match {
        case(_, Nil) => throw new NoSuchElementException()
        case(0, x :: xs) => (shifted.reverse ::: xs, x)
        case(_, x :: xs) => removeAtTR(i - 1, x :: shifted, xs)
      }
  
    removeAtTR(idx, Nil, list)
  }
}

