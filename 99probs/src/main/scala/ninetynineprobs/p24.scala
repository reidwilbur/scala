
import p23._

object p24 {
  def lotto(n: Int, maxNum: Int): List[Int] = {
    val r = new Range(1, maxNum+1, 1)

    return randomSelect(n, r.toList)
  }
}

