
def isPalindrome[T](list: List[T]): Boolean = {
  def isPalindromeTR[T](lastCheck: Boolean, list: List[T]): Boolean = {
    if (list == Nil || list.size == 1)
      lastCheck
    else
      isPalindromeTR(list.head == list.last, list.init.tail)
  }

  isPalindromeTR(true, list)
}

val testVals = List(
  List(1, 2, 3, 2, 1),
  List(1, 1, 3, 2, 1),
  List(1, 2, 2, 1),
  List(1, 1, 2, 1),
  List(1)
)

testVals.foreach(list => println("isPalindrome("+list+") = "+isPalindrome(list)))
