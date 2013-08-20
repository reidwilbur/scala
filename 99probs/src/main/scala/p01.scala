
package ninetynineprobs {
  object p01 {
    def last[T](list: List[T]): T = list match {
      case List() => throw new RuntimeException("No last element for empty list")
      case x :: List() => x
      case x :: xs => last(xs)
    }
  }
}

