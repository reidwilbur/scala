
def pack[T](list: List[T]): List[List[T]] = {
  def packTR[T](packed: List[List[T]], flat: List[T]): List[List[T]] = 
    flat match {
      case List() => packed.reverse
      case _ =>
        val (pack, process) = flat.span(el => el == flat.head)
        packTR(pack :: packed, process)
    }

  packTR(Nil, list)
}

val p = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

println(p)
assert(p == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
