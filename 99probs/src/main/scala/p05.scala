
package ninetynineprobs {
  object p05 {
    def reverse[T](list: List[T]): List[T] = {
      def reverseAcc[T](revList: List[T], list: List[T]): List[T] = 
        list match {
          case List() => revList
          case x :: xs => reverseAcc(x :: revList, xs)
        }
    
      reverseAcc(Nil, list)
    }
  }
}

