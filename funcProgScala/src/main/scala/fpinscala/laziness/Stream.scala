package fpinscala.laziness

import Stream._

sealed abstract class Stream[+A] { // The abstract base class for streams. It will have only two sub-classes, one for the empty stream and another for the nonepty stream.
  def uncons: Option[Cons[A]] // The abstract interface of `Stream` is simply this one abstract method, `uncons`. Calling `uncons` will give us either `None` if it's empty or `Some` if it's nonempty. In the nonempty case, a `Cons` consists of the head and the tail.
  def isEmpty: Boolean = uncons.isEmpty
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    uncons match {
      case Some(c) => f(c.head, c.tail.foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case None => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] =
    foldRight(Nil: List[A]){ (a, la) => a :: la }

  def take(n: Int): Stream[A] = 
    //if (n > 0) this match {
    //  case c: Cons[_] => Stream.cons(c.head, c.tail.take(n-1))
    //  case _ => Stream.empty
    //}
    //else Stream.empty
    Stream.unfold((this,n)){ s => s match {
      case (c: Cons[_], nn) if nn > 0 => Some((c.head, (c.tail, nn-1)))
      case _ => None
    }}

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def dropRec(s: Stream[A], d: Int): Stream[A] = {
      if (d > 0) s match {
        case c: Cons[_] => dropRec(c.tail, d-1)
        case _ => Stream.empty
      }
      else s
    }

    dropRec(this, n)
  }

  def takeWhile(f: A => Boolean): Stream[A] = 
    //this match {
    //  case c: Cons[_] if f(c.head) => Stream.cons(c.head, c.tail.takeWhile(f))
    //  case _ => Stream.empty
    //}
    //foldRight(Stream.empty: Stream[A]){ (a, sa) => if (f(a)) Stream.cons(a, sa) else Stream.empty }
    unfold(this){ s => s match {
      case (c: Cons[_]) if f(c.head) => Some((c.head, c.tail))
      case _ => None
    }}

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true){ (a, b) => p(a) && b }

  def headOption: Option[A] = 
    foldRight(None: Option[A]){ (a, oa) => Some(a) }

  def map[B](f: A => B): Stream[B] = 
    //foldRight(Stream.empty: Stream[B]){ (a, lb) => Stream.cons(f(a), lb) }
    Stream.unfold(this){ s => s match {
      case c: Cons[_] => Some((f(c.head), c.tail))
      case _ => None
    }}

  def filter(f: A => Boolean): Stream[A] = 
    foldRight(Stream.empty: Stream[A]){ (a, sa) => if (f(a)) Stream.cons(a, sa) else sa }

  def append[AA >: A](s: => Stream[AA]): Stream[AA] =
    foldRight(s){ (a, sa) => Stream.cons(a, sa) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty: Stream[B]){ (a, sb) => f(a).append(sb) }

  def find(f: A => Boolean): Option[A] =
    filter(f).headOption

  def zipWith[B,C](other: Stream[B])(f: (A,B) => C): Stream[C] = 
    unfold((this, other)){ s => s match {
      case (t: Cons[_], o: Cons[_]) => Some( ( f(t.head,o.head), (t.tail,o.tail) ) )
      case _ => None
    }}

  def zipAll[B](other: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this,other)){ s => s match {
      case (Empty, Empty) => None
      case (t, o) => Some( ( (t.headOption, o.headOption), (t.drop(1), o.drop(1)) ) )
    }}

  def startsWith[A](s: Stream[A]): Boolean = {
    !zipWith(s)((a1,a2) => a1 == a2).exists(_ == false)
  }
}

object Empty extends Stream[Nothing] {
  val uncons = None // A concrete implementation of `uncons` for the empty case. The empty stream is represented by `None`. Note the use of a `val` in this concrete implementation.
}

sealed abstract class Cons[+A] extends Stream[A] { // A nonempty stream consists of a head and a tail and its `uncons` implementation is simply itself wrapped in `Some`.
  def head: A // Note that head and tail are abstract. A concrete implementation is given by the `cons` method in the Stream companion object.
  def tail: Stream[A]
  val uncons = Some(this)
}

object Stream {
    def empty[A]: Stream[A] = Empty // A "smart constructor" for creating an empty stream of a particular type.
  
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = new Cons[A] { // A "smart constructor" for creating a nonempty stream.
      lazy val head = hd // The head and tail are implemented by lazy vals.
      lazy val tail = tl
    }
  
    def apply[A](as: A*): Stream[A] = // A convenient variable-argument method for constructing a `Stream` from multiple elements.
      if (as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  //val ones: Stream[Int] = cons(1, ones)
  def ones: Stream[Int] = 
    unfold(1)(_ => Some(1,1))

  def constant[A](a: => A): Stream[A] = 
  //{
  //  lazy val tail: Stream[A] = cons(a, tail)
  //  tail
  //}
    unfold(a)(_ => Some(a,a))

  def from(n: Int): Stream[Int] = 
  //  cons(n, from(n+1))
    unfold(n)(n => Some(n, n+1))

  val fibs: Stream[Int] = 
  //{
  //  def calcFibs(f1: Int, f2: Int): Stream[Int] = 
  //    cons(f1, calcFibs(f2, f1+f2))
  //  calcFibs(0,1)
  //}
    unfold((0,1))(s => Some((s._1, (s._2, s._1 + s._2))) )
  
  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = 
    //f(z).fold(Stream.empty: Stream[A])( a_s => cons(a_s._1, unfold(a_s._2)(f)) )
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
}

