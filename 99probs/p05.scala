
def reverse[T](list: List[T]): List[T] = {
  def reverseAcc[T](revList: List[T], list: List[T]): List[T] = {
    if (list == Nil)
      revList
    else
      reverseAcc(list.head :: revList, list.tail)
  }

  reverseAcc(Nil, list)
}

val r = reverse(List(1, 1, 2, 3, 5, 8))
println(r)
assert(r == List(8, 5, 3, 2, 1, 1))
