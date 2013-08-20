
package ninetynineprobs {
  object p16 {
    def drop[T](n: Int, list: List[T]): List[T] = {
      if (n <= 0) throw new IllegalArgumentException("n must be > 0")
      
      def dropTR[T](r: Int, keepers: List[T], list: List[T]): List[T] =
        (r, list) match {
          case (_, List()) => 
            keepers.reverse
    
          case (1, _ :: xs) =>
            dropTR(n, keepers, xs)
    
          case (_, x :: xs) => 
            dropTR(r-1, x :: keepers, xs)
        }
    
      dropTR(n, Nil, list)
    }
  }
}

