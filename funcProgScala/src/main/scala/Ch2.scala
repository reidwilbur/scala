
object CH2 {

  def abs(n: Int): Int = {
     if (n < 0) -n
     else n
  }

  def fib(n: Int): Int = {
    def go(cn: Int, n1: Int, n2: Int): Int = {
      val f = n1+n2;
      if (cn == n) f
      else go(cn+1, f, n1)
    }
    if (n == 0) 0
    else if (n == 1) 1
    else go(2, 1, 0)
  }

  private def formatAbs(x: Int) = {
    val msg = "The abs value of %d is %d."
    msg.format(x, abs(x))
  }

  private def formatFib(n: Int) = {
    val msg = "Fib of %d is %d."
    msg.format(n, fib(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
    println(formatResult("fib", 7, fib))
  }
}

