package ninetynineprobs

object p26 {
  def combinations[T](k: Int, list: List[T]): List[List[T]] = {
    def genSubLists[T](list: List[T], sublists: List[List[T]]): List[List[T]] = {
      list match {
        case Nil => sublists
        case x :: xs => genSubLists(xs, list :: sublists)
      }
    }

    def genCombis[T](currDepth: Int, sublist: List[T], stack: List[T]): List[List[T]] = {

      sublist match {
        case Nil => 
          Nil
        case sl if (currDepth == k-1) =>
          val r = sl.map { el => (el :: stack).reverse }
          r
        case sl =>
          val sublists = genSubLists(sl, Nil) 
          sublists.flatMap { ell => genCombis(currDepth+1, ell.tail, ell.head :: stack) }
      }
    }

    genCombis(0, list, Nil)
  }
}

