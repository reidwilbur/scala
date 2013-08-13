
def last[T](list: List[T]): T = list match {
  case List() => throw new RuntimeException("No last element for empty list")
  case x :: List() => x
  case x :: xs => last(xs)
}

val l = last(List(1, 1, 2, 3, 5, 8))
println(l)
assert(8 == l)
