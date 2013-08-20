
def duplicateN[T](n: Int, list: List[T]): List[T] = {
  list.flatMap(el => List.fill(n)(el))
}

val dupn = duplicateN(3, List('a, 'b, 'c, 'c, 'd))
println(dupn)
assert(dupn == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
