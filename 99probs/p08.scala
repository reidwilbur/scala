
def compress(list: List[Any]): List[Any] = {
  def compressTR[T](compList: List[T], list: List[T]): List[T] = {
    if (list == Nil) {
      compList.reverse
    }
    else if (compList.head == list.head) {
      compressTR(compList, list.tail)
    }
    else {
      compressTR(list.head +: compList, list.tail)
    }
  }

  compressTR(list.head +: Nil, list.tail)
}

val l = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
println(l)
assert(List('a, 'b, 'c, 'a, 'd, 'e) == l)
