
//Given two arrays, sorted and exactly the same length, 
//write a function that finds a pair of numbers, 
//one from each of the arrays, such that the difference 
//between both is as small as possible.
//
//Input:
//[0, 3, 5, 8, 10], [6, 9, 12, 13, 14]
//Output:
//[5, 6]

//requires a sorted list with no repeated vals
//binary search by pairs, log(n) runtime
def getNearestValue(i: Int, list: Iterable[Int]): Int = {
  if (list.size == 1) {
    list.head
  }
  else {
    val mid = list.size/2
    val bot = list.dropRight(mid+(list.size % 2))
    val top = list.drop(mid)
    if (math.abs(i - bot.last) <= math.abs(i - top.head))
      getNearestValue(i, bot)
    else
      getNearestValue(i, top)
  }
}

def getMinDiffElements(list1: Iterable[Int], list2: Iterable[Int]): (Int, Int) = {
  if (list1.size == 1) {
    (list1.head, getNearestValue(list1.head, list2))
  }
  else {
    val i = list1.head
    val neighbor = getNearestValue(i, list2)

    val nextPair = getMinDiffElements(list1.tail, list2)
    if (math.abs(i - neighbor) <= math.abs(nextPair._1 - nextPair._2))
      (i, neighbor)
    else
      nextPair
  }
}

val testVals = List(
  (Array(0, 3, 5, 8, 10),    Array(6, 9, 12, 13, 14)),
  (Array(7, 12, 15, 20, 21), Array(1, 5, 9, 13, 17)),
  (Array(6, 10, 15, 18, 21), Array(16, 17, 18, 23, 27))
)

testVals.foreach(lists => {
  println("["+lists._1.mkString(", ")+"], ["+lists._2.mkString(", ")+"]")

  val minDiff = getMinDiffElements(lists._1, lists._2)

  println("["+minDiff._1+","+minDiff._2+"]")
  println()
})

