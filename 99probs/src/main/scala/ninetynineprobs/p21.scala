
object p21 {
  def insertAt[T](newEl: T, idx: Int, list: List[T]): List[T] = {
    val (front, back) = list.splitAt(idx)
    front ::: newEl :: back
  }
}

