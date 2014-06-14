package fpinscala.errorhandling

sealed trait OOption[+A] {
  def map[B](f: A => B): OOption[B] = 
    this match {
      case SSome(a) => SSome(f(a))
      case _ => NNone
    }

  def getOrElse[B >: A](default: => B): B = 
    this match {
      case SSome(a) => a
      case _ => default
    }

  def flatMap[B](f: A => OOption[B]): OOption[B] = 
    map(f).getOrElse(NNone)
    
  def orElse[B >: A](ob: => OOption[B]): OOption[B] = 
    map(a => this).getOrElse(ob)
    

  def filter(f: A => Boolean): OOption[A] = 
    flatMap(a => if (f(a)) SSome(a) else NNone)
}
case class SSome[+A](get: A) extends OOption[A]
case object NNone extends OOption[Nothing]


object OOption {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): OOption[Double] =
    if (xs.isEmpty) NNone
    else SSome(xs.sum / xs.length)
}
