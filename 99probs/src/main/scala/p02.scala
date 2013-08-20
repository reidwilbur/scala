
package ninetynineprobs {

  object p02 {
    def penultimate[T](list: List[T]): T = list match {
      case List() => throw new RuntimeException("No penultimate for empty list")
      case x :: List() => x
      case x :: _ :: List() => x
      case x :: xs => penultimate(xs)
    }
  }
}

