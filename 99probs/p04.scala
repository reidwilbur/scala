
def length[T](list: List[T]): Int = {
  def lengthAcc[T](lenAcc: Int, list: List[T]): Int = 
    list match{
      case List() => lenAcc
      case x :: xs => lengthAcc(lenAcc+1, xs)
    }
  
  lengthAcc(0, list)
}

val l = length(List(1, 1, 2, 3, 5, 8)) 
println(l)
assert(l == 6)
