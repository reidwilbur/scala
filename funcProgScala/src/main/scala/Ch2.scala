
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

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def go(idx: Int): Boolean = {
      if (idx + 1 >= as.length) true
      else {
        if (gt(as(idx), as(idx+1))) false
        else go(idx+1)
      }
    }
    go(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = 
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)

    loop(0)
  }

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a) => (b) => f(a,b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B) : A => C = {
    (a) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
    println(formatResult("fib", 7, fib))

    def gt[T <: Int](a: T, b: T): Boolean = {
      a > b
    }

    println(isSorted(Array(1), gt))
    println(isSorted(Array(1, 2, 0), gt))
    println(isSorted(Array(1, 2, 3), gt))
  }
}

