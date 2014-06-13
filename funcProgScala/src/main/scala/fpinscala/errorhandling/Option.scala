package fpinscala.errorhandling

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = 
    this match {
      case MySome(a) => MySome(f(a))
      case _ => MyNone
    }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = 
    this match {
      case MySome(a) => f(a)
      case _ => MyNone
    }
    //map(f).map(ob => ob.get)
    
  def getOrElse[B >: A](default: => B): B = 
    this match {
      case MySome(a) => a
      case _ => default
    }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
    this match {
      case MySome(a) => this
      case _ => ob
    }
    //flatMap(a => this).getOrElse(orelse)
  }
    

  def filter(f: A => Boolean): MyOption[A] = 
    flatMap(a => if (f(a)) MySome(a) else MyNone)
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]


object MyOption {
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

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)
}
