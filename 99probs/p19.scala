
def rotate[T](n: Int, list: List[T]): List[T] = {
  def rotateLeft(i: Int, shifted: List[T], src: List[T]): List[T] = 
    (i, src) match {
      case (_, List()) => src ::: shifted.reverse
      case (0, _) => src ::: shifted.reverse
      case (_, x :: xs) => rotateLeft(i - 1, x :: shifted, xs)
    }

  val rotleft = 
    if (n >= 0) n % list.length
    else list.length - ((n * -1) % list.length)

  rotateLeft(rotleft, Nil, list)
}

val l = rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
val r = rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
val s = rotate(5, List('a, 'b, 'c, 'd))
val t = rotate(-6, List('a, 'b, 'c, 'd))

println(l)
assert(l == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))

println(r)
assert(r == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))

println(s)
assert(s == List('b, 'c, 'd, 'a))

println(t)
assert(t == List('c, 'd, 'a, 'b))
