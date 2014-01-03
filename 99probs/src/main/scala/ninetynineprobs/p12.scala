package ninetynineprobs

object p12 {
  def decode[T](rle: List[(Int, T)]): List[T] = {
    rle.flatMap(el => { List.fill(el._1)(el._2) } )
  }
}

