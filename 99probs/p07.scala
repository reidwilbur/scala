
def flatten(nestedList: List[Any]): List[Any] = {
  nestedList flatMap {
    case list: List[_] => flatten(list)
    case element => List(element)
  }
}

println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
