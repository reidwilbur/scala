
def decode[T](rle: List[(Int, T)]): List[T] = {
  rle.flatMap(el => { List.fill(el._1)(el._2) } )
}

val decoded = decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) 
println(decoded)
assert(decoded == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
