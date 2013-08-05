
def pack[T](list: List[T]): List[List[T]] = {
  def packTR[T](packed: List[List[T]], packing: List[T], flat: List[T]): List[List[T]] = {
    if (flat == Nil && packing == Nil)
      packed.reverse
    else if (flat == Nil)
      ((packing.reverse) +: packed).reverse
    else if (packing == Nil)
      packTR(packed, flat.head +: Nil, flat.tail)
    else if (packing.head == flat.head)
      packTR(packed, flat.head +: packing, flat.tail)
    else
      packTR((packing.reverse) +: packed, flat.head +: Nil, flat.tail)
  }

  packTR(Nil, Nil, list)
}

def packspan[T](list: List[T]): List[List[T]] = {
  def packTR[T](packed: List[List[T]], flat: List[T]): List[List[T]] = {
    if (flat == Nil)
      packed.reverse
    else {
      val (pack, process) = flat.span(el => el == flat.head)
      packTR((pack) +: packed, process)
    }
  }

  packTR(Nil, list)
}


val p = packspan(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))

println(p)
assert(p == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
