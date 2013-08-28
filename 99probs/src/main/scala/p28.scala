package ninetynineprobs {
  object p28 {
    def lsort[T](list: List[List[T]]): List[List[T]] = {
      list.sortWith { _.length < _.length }
    }

    def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
      def lsortFreqTR[T](listsByLen: Map[Int, List[List[T]]], listOfLists: List[List[T]]): List[List[T]] = {
        listOfLists match {
          case Nil => 
            val listsInIncLenFreq = 
              listsByLen.values.toList.sortWith{ _.length < _.length }

            listsInIncLenFreq.flatMap(lists => { lists.reverse })

          case list :: rest =>
            val len = list.length

            val lenList = 
              if(listsByLen.contains(len))
                listsByLen(len)
              else
                List()

            lsortFreqTR(listsByLen + (len -> (list :: lenList)), rest)
        }
      }

      lsortFreqTR(Map.empty, list)
    }
  }
}
