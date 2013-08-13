
def duplicate[T](list: List[T]): List[T] =
  list match {
    case List() => List()
    case x :: xs =>
      x :: x :: duplicate(xs)
  }

def dupflatmap[T](list: List[T]): List[T] =
  list.flatMap(el => List(el,el))

val dup = duplicate(List('a, 'b, 'c, 'c, 'd))
println(dup)
assert(dup == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))

val fdup = dupflatmap(List('a, 'b, 'c, 'c, 'd))
assert(fdup == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
