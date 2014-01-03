package ninetynineprobs

object p03 {
   def nth[T](idx: Int, list: List[T]): T = list match {
     case List() => throw new RuntimeException("Not enough elements in list")
     case x :: _ if idx == 0 => x
     case x :: xs => nth(idx-1, xs)
   }
}

