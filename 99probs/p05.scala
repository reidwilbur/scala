
def reverse[T](list: List[T]): List[T] = {
  def reverseAcc[T](revList: List[T], list: List[T]): List[T] = {
    if (list == Nil)
      revList
    else
      reverseAcc(list.head :: revList, list.tail)
  }

  reverseAcc(Nil, list)
}

println(reverse(List(1, 1, 2, 3, 5, 8)))
