
def nth[T](idx: Int, list: List[T]): T = {
  if (idx == 0) 
    list.head
  else
    nth(idx-1, list.tail)
}

println(nth(2, List(1, 1, 2, 3, 5, 8)))
