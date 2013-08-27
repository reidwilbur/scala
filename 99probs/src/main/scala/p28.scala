package ninetynineprobs {
  object p28 {
    def lsort[T](list: List[List[T]]): List[List[T]] = {
      def lsortTR[T](listsByLen: Map[Int, List[T]], currList: List[List[T]]): List[List[T]] = {
        currList match {
          case Nil => 
            listsByLen.keySet().toList().sorted().flatMap { key => listsByLen(key) }
          case x :: xs =>
            val l = x.length
            val m = listsByLen(l) match {
              case None =>
                listsByLen += (l -> x :: Nil)
              case _ =>
                listsByLen += (l -> x :: listsByLen(l)) 
            }
            lsortTR(m, xs)
        }
      }

      lsortTR(Map.empty, list)
    }
  }
}
