
def penultimate[T](list: List[T]): T = {
  if (list.size <= 2)
    list.head
  else
    penultimate(list.tail)
}

println(penultimate(List(1, 1, 2, 3, 5, 8)))
