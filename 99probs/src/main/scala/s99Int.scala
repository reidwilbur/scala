package ninetynineprobs {
  class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean = {
      def isPrimeTR(i: Int): Boolean = 
        i match {
          case 1 => true
          case x if (start % x != 0) =>
            isPrimeTR(x - 1)
          case _ =>
            false
        }
     
      start match {
        case 0 => false
        case 1 => true
        case _ => isPrimeTR(start.abs - 1)
      }
    }

    def isCoPrimeTo(i: Int): Boolean =
      gcd(start, i) == 1

    def totient: Int = {
      (1 until this.start).filter{ this.isCoPrimeTo(_) }.length
    }

    def primeFactors: List[Int] = {
      if (this.isPrime)
        List(this.start)
      else {
      }
      
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(a: Int, b: Int): Int = {
      val r = a % b
  
      if (r == 0)
        b
      else
        gcd(b, r)
    }

  }
}
