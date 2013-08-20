
package ninetynineprobs {
  object p22 {
    def range(start: Int, end: Int): List[Int] = {
      def rangeTR(curr: Int, r: List[Int]): List[Int] =
        curr match {
          case _ if curr == start => start :: r
          case x => rangeTR(curr - 1 , x :: r)
        }
    
      if (start > end) throw new IllegalArgumentException("Start must be less than end")
    
      rangeTR(end, Nil)
    }
  }
}

