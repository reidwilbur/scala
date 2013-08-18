
def insertAt[T](newEl: T, idx: Int, list: List[T]): List[T] = {
  val (front, back) = list.splitAt(idx)
  front ::: newEl :: back
}

val i = insertAt('new, 1, List('a, 'b, 'c, 'd))
println(i)
assert(i == List('a, 'new, 'b, 'c, 'd))


val j = insertAt('new, 32, List('a, 'b, 'c, 'd))
println(j)
assert(j == List('a, 'b, 'c, 'd, 'new))
