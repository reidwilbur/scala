
def encode[T](list: List[T]): List[(Int, T)] = {
  def packTR[T](packed: List[(Int, T)], flat: List[T]): List[(Int, T)] = 
    flat match {
      case List() => packed.reverse
      case _ =>
        val(pack, process) = flat.span(el => el == flat.head)
        packTR((pack.length, pack.head) :: packed, process)
    }

  packTR(Nil, list)
}

val rle = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
println(rle)
assert(rle == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
