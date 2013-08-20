
def drop[T](n: Int, list: List[T]): List[T] = {
  if (n <= 0) throw new IllegalArgumentException("n must be > 0")
  
  def dropTR[T](r: Int, keepers: List[T], list: List[T]): List[T] =
    (r, list) match {
      case (_, List()) => 
        keepers.reverse

      case (1, _ :: xs) =>
        dropTR(n, keepers, xs)

      case (_, x :: xs) => 
        dropTR(r-1, x :: keepers, xs)
    }

  dropTR(n, Nil, list)
}

val d = drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
println(d)
assert(d == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
