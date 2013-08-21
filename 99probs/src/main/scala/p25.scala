
package ninetynineprobs {
  object p25 {
    import p23._

    def randomPermute[T](list: List[T]): List[T] = 
      randomSelect(list.length, list)

    def randomPermuteFast[T : ClassManifest](list: List[T]): List[T] = {
      val rndm = new util.Random(System.currentTimeMillis)

      val arr = list.toArray

      for(idx <- 0 until arr.length) {
        val swapIdx = rndm.nextInt(arr.length)
        val tmp = arr(swapIdx)
        arr(swapIdx) = arr(idx)
        arr(idx) = tmp
      }

      arr.toList
    }
  }
}

