import org.scalatest.FunSuite

package ninetynineprobs {

  class p38test extends FunSuite {
    import S99Int._

    def execTime[T](f: => T): (T, Long) = {
      val st = System.nanoTime
      val ret = f
      (ret, System.nanoTime - st)
    }

    test("totient runtimes") {
      val n: Int = 10090

      for(i <- 0 until 20) {
        val (r, runtime0) = execTime {
          primes.takeWhile{ _ <= math.sqrt(n)}.force
        }
        
        val (totient1, runtime1) = execTime(n.totient)
        val (totient2, runtime2) = execTime(n.totientFromPrimeFactors)
  
        assert(totient1 == totient2)
        println("preload   = "+runtime0/1000+"us")
        println("totient   = "+runtime1/1000+"us")
        println("totientpf = "+runtime2/1000+"us")
        val perf = runtime1/runtime2.toDouble
        println("perf      = "+"%.2f".format(perf))
      }
    }

  }

}


