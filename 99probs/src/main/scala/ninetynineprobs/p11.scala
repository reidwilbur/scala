
object p11 {
  def encode[T](list: List[T]): List[Any] = {
    def packTR[T](packed: List[Any], flat: List[T]): List[Any] = 
      flat match {
        case List() => packed.reverse
        case _ =>
          val (pack, process) = flat.span(el => el == flat.head)
          pack match {
            case x :: List() =>
              packTR(x :: packed, process)
            case _ =>
              packTR((pack.length, pack.head) :: packed, process)
          }
    }
  
    packTR(Nil, list)
  }
}

