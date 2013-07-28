
def length[T](list: List[T]): Int = {
  def lengthAcc[T](lenAcc: Int, list: List[T]): Int = {
    if (list == Nil)
      lenAcc
    else
      lengthAcc(lenAcc + 1, list.tail)
  }
  
  lengthAcc(0, list)
}

println(length(List(1, 1, 2, 3, 5, 8)))
