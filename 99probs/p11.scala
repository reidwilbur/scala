
def encode[T](list: List[T]): List[Any] = {
  def packTR[T](packed: List[Any], flat: List[T]): List[Any] = {
    if (flat == Nil)
      packed.reverse
    else {
      val (pack, process) = flat.span(el => el == flat.head)
      if (pack.length == 1)
        packTR(pack.head +: packed, process)
      else
        packTR((pack.length, pack.head) +: packed, process)
    }
  }

  packTR(Nil, list)
}

val rle = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
println(rle)
assert(rle == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
