
def penultimate[T](list: List[T]): T = list match {
  case List() => throw new RuntimeException("No penultimate for empty list")
  case x :: List() => x
  case x :: _ :: List() => x
  case x :: xs => penultimate(xs)
}

val e = penultimate(List(1, 1, 2, 3, 5, 8))
println(e)
assert(e == 5)
