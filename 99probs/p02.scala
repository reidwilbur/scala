
def penultimate[T](list: List[T]): T = {
  if (list.size <= 2)
    list.head
  else
    penultimate(list.tail)
}

val e = penultimate(List(1, 1, 2, 3, 5, 8))
println(e)
assert(e == 5)
