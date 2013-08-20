
package ninetynineprobs {
  object p18 {
    def slice[T](start: Int, until: Int, list: List[T]): List[T] = {
      def sliceTR[T](s: Int, len: Int, sliced: List[T], list: List[T]): List[T] =
        (s, len, list) match {
          case (_, _, List())  => sliced.reverse
          case (_, 0, x :: xs) => sliced.reverse
          case (0, _, x :: xs) => sliceTR(0, len - 1, x :: sliced, xs)
          case (_, _, x :: xs) => sliceTR(s - 1, len, sliced, xs)
        }
    
      sliceTR(start, until - start, Nil, list)
    }
  }
}

