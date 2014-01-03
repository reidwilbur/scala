
object p14 {
  def duplicate[T](list: List[T]): List[T] =
    list match {
      case List() => List()
      case x :: xs =>
        x :: x :: duplicate(xs)
    }

  def dupflatmap[T](list: List[T]): List[T] =
    list.flatMap(el => List(el,el))
}

