package ninetynineprobs

object p24 {
  def lotto(n: Int, maxNum: Int): List[Int] = {
    val r = new Range(1, maxNum+1, 1)

    return p23.randomSelect(n, r.toList)
  }
}

