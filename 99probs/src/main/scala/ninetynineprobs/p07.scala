package ninetynineprobs

object p07 {
   def flatten(nestedList: List[Any]): List[Any] = {
     nestedList flatMap {
        case list: List[_] => flatten(list)
        case element => List(element)
     }
   }
}

