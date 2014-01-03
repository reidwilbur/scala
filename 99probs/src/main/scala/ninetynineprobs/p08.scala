package ninetynineprobs

object p08 {
   def compress(list: List[Any]): List[Any] = {
     def compressTR[T](compList: List[T], list: List[T]): List[T] = 
        list match {
           case List() => compList.reverse
           case x :: xs if (x != compList.head) => compressTR(x :: compList, xs)
           case x :: xs => compressTR(compList, xs)
        }
        
     list match {
        case List() => List()
        case _ => compressTR(list.head :: Nil, list)
     }
   }
}

