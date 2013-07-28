
def last[T](list: List[T]): T = {
  if (list.size == 1) 
    list.head
  else
    last(list.tail)
}

println(last(List(1, 1, 2, 3, 5, 8)))
