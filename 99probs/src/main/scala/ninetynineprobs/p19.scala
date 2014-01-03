
object p19 {
  def rotate[T](n: Int, list: List[T]): List[T] = {
    def rotateLeft(i: Int, shifted: List[T], src: List[T]): List[T] = 
      (i, src) match {
        case (_, List()) => src ::: shifted.reverse
        case (0, _) => src ::: shifted.reverse
        case (_, x :: xs) => rotateLeft(i - 1, x :: shifted, xs)
      }
  
    val rotleft = 
      if (n >= 0) n % list.length
      else list.length - ((n * -1) % list.length)
  
    rotateLeft(rotleft, Nil, list)
  }
}

