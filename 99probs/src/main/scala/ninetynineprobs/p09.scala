package ninetynineprobs

object p09 {
   def pack[T](list: List[T]): List[List[T]] = {
     def packTR[T](packed: List[List[T]], flat: List[T]): List[List[T]] = 
        flat match {
           case List() => packed.reverse
           case _ =>
           val (pack, process) = flat.span(el => el == flat.head)
           packTR(pack :: packed, process)
        }
     
     packTR(Nil, list)
   }
}

