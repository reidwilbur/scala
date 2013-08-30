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
      def findPrimeFactors(dividend: Int, primes: Stream[Int], primeFactors: List[Int]): List[Int] = {
        val currPrime = primes.head
        val quotient = dividend / currPrime
        val rem = dividend % currPrime

        (quotient, rem) match {
          case (1, 0) => 
            (currPrime :: primeFactors).reverse
          case (_, 0) =>
            findPrimeFactors(quotient, primes, currPrime :: primeFactors)
          case (_, _) =>
            findPrimeFactors(dividend, primes.tail, primeFactors)
        }
      }

      findPrimeFactors(this.start, primes, Nil)
    }

    def primeFactorMultiplicity: Map[Int, Int] = {
      def collectFactors(factors: List[Int], factorCounts: Map[Int, Int]): Map[Int, Int] = 
        factors match {
          case Nil => factorCounts
          case f :: rest =>
            val count = 
              if (factorCounts.contains(f)) 
                factorCounts(f) + 1 
              else 
                1
            collectFactors(rest, factorCounts + (f -> count))
        }
      
      collectFactors(this.primeFactors, Map.empty)
    }

    def totientFromPrimeFactors: Int = {
      this.primeFactorMultiplicity.foldLeft(1)( 
        (totientAcc, entry) => {
          val factor = entry._1
          val count  = entry._2
          totientAcc*(factor-1)*(math.pow(factor, count-1).toInt)
        } 
      )
    }

  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    
    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

    def gcd(a: Int, b: Int): Int = {
      val r = a % b
  
      if (r == 0)
        b
      else
        gcd(b, r)
    }

  }
}
