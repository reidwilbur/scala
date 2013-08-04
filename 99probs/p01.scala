
def last[T](list: List[T]): T = {
  if (list.size == 1) 
    list.head
  else
    last(list.tail)
}

val l = last(List(1, 1, 2, 3, 5, 8))
println(l)
assert(8 == l)
