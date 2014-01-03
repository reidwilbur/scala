
object p15 {
  def duplicateN[T](n: Int, list: List[T]): List[T] = {
    list.flatMap(el => List.fill(n)(el))
  }
}

