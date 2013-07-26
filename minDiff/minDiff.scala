
//Given two arrays, sorted and exactly the same length, 
//write a function that finds a pair of numbers, 
//one from each of the arrays, such that the difference 
//between both is as small as possible.
//
//Input:
//[0, 3, 5, 8, 10], [6, 9, 12, 13, 14]
//Output:
//[5, 6]

//binary search by pairs, log(n) runtime
def getNearestValue(i: Int, list: Array[Int]): Int = {
  if (list.length == 1) {
    return list(0)
  }
  else {
    val midIdx = (list.length-1)/2
    if (math.abs(i - list(midIdx)) < math.abs(i - list(midIdx+1)))
      getNearestValue(i, list.splitAt(midIdx+1)._1)
    else
      getNearestValue(i, list.splitAt(midIdx+1)._2)
  }
}

def getMinDiffElements(list1: Array[Int], list2: Array[Int]): (Int, Int) = {
  //maximum bad functional style, using var instead of val...
  var list1val = Int.MinValue
  var list2val = Int.MinValue
  var bestDiff = Int.MaxValue

  //iterate over n elements
  for(i <- list1) {
    //log(n) search for neighbor
    val neighbor = getNearestValue(i, list2)

    //some constant time stuff c
    val diff = math.abs(i - neighbor)
    if (bestDiff > diff) {
      bestDiff = diff
      list1val = i
      list2val = neighbor
    }
  }
  //for a grand total of n(log(n) + c) = nlog(n)
  return (list1val, list2val)
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

