package fpinscala.errorhandling

sealed trait EEither[+E, +A] {
  def map[B](f: A => B): EEither[E, B] = 
    this match {
      case RRight(a) => RRight(f(a))
      case LLeft(e) => LLeft(e)
    }

  def flatMap[EE >: E, B](f: A => EEither[EE, B]): EEither[EE, B] = 
    this match {
      case RRight(a) => f(a)
      case LLeft(e) => LLeft(e)
    }

  def orElse[EE >: E, B >: A](b: => EEither[EE, B]): EEither[EE, B] =
    this match {
      case RRight(a) => RRight(a)
      case _ => b
    }

  def map2[EE >: E, B, C](b: EEither[EE, B])(f: (A, B) => C): EEither[EE, C] =
    flatMap(a => b.map(bb => f(a,bb)))
}
case class LLeft[+E](value: E) extends EEither[E, Nothing]
case class RRight[+A](value: A) extends EEither[Nothing, A]
 
object EEither {

}
