
def flatten(nestedList: List[Any]): List[Any] = {
  nestedList flatMap {
    case list: List[_] => flatten(list)
    case element => List(element)
  }
}

val flat = flatten(List(List(1, 1), 2, List(3, List(5, 8))))
println(flat)
assert(flat == List(1, 1, 2, 3, 5, 8))
