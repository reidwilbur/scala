package ninetynineprobs

object p06 {
   def isPalindrome[T](list: List[T]): Boolean = {
     def isPalindromeTR[T](lastCheck: Boolean, list: List[T]): Boolean = 
        list match {
           case List() => lastCheck
           case x :: List() => lastCheck
           case _ => isPalindromeTR(list.head == list.last, list.init.tail)
        }
     
     isPalindromeTR(true, list)
   }
}

