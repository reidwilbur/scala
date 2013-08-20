
def compress(list: List[Any]): List[Any] = {
  def compressTR[T](compList: List[T], list: List[T]): List[T] = 
    list match {
      case List() => compList.reverse
      case x :: xs if (x != compList.head) => compressTR(x :: compList, xs)
      case x :: xs => compressTR(compList, xs)
    }
    
  list match {
    case List() => List()
    case _ => compressTR(list.head :: Nil, list)
  }
}

val l = compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
println(l)
assert(List('a, 'b, 'c, 'a, 'd, 'e) == l)

assert(compress(Nil) == Nil)
assert(compress(List('b)) == List('b))
