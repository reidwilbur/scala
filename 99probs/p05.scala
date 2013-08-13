
def reverse[T](list: List[T]): List[T] = {
  def reverseAcc[T](revList: List[T], list: List[T]): List[T] = 
    list match {
      case List() => revList
      case x :: xs => reverseAcc(x :: revList, xs)
    }

  reverseAcc(Nil, list)
}

val r = reverse(List(1, 1, 2, 3, 5, 8))
println(r)
assert(r == List(8, 5, 3, 2, 1, 1))
