
package ninetynineprobs {
  object p10 {
    def encode[T](list: List[T]): List[(Int, T)] = {
      def packTR[T](packed: List[(Int, T)], flat: List[T]): List[(Int, T)] = 
        flat match {
          case List() => packed.reverse
          case _ =>
            val(pack, process) = flat.span(el => el == flat.head)
            packTR((pack.length, pack.head) :: packed, process)
        }
    
      packTR(Nil, list)
    }
  }
}

