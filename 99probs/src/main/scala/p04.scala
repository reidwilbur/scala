
package ninetynineprobs {
  object p04 {
    def length[T](list: List[T]): Int = {
      def lengthAcc[T](lenAcc: Int, list: List[T]): Int = 
        list match{
          case List() => lenAcc
          case x :: xs => lengthAcc(lenAcc+1, xs)
        }
      
      lengthAcc(0, list)
    }
  }
}

