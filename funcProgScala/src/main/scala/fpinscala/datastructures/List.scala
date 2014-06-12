package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](as: List[A]): List[A] = 
    as match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

  def setHead[A](h: A, as: List[A]): List[A] =
    Cons(h, tail(as))

  def drop[A](l: List[A], n: Int): List[A] = 
    l match {
      case Nil => Nil
      case _ if n == 0 => l
      case Cons(_, tail) => drop(tail, n-1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = 
    l match {
      case Nil => Nil
      case Cons(h, tail) if f(h) => dropWhile(tail)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = {
    def itr(from: List[A], acc: List[A]): List[A] = {
      from match {
        case Nil => Nil
        case Cons(h, Nil) => acc
        case Cons(h, tail) => itr(tail, append(acc, Cons(h, Nil)))
      }
    }
    itr(l, Nil)
  }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x: A, len: Int) => len+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumFoldLeft(ns: List[Int]): Int = 
    foldLeft(ns, 0)( (z, x) => z + x )

  def prodFoldLeft(ns: List[Double]): Double = 
    foldLeft(ns, 1.0)( (z, x) => z * x )

  def lengthFoldLeft[A](l: List[A]): Int = 
    foldLeft(l, 0)( (z, x) => z + 1 )

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, Nil: List[A])( (z, xs) => Cons(xs, z) )

  def foldRightWithFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), z){(b, a) => f(a, b)}

  def appendWithFoldRight[A](l: List[A], r: List[A]): List[A] = 
    foldRight(l, r){ (x, z) => Cons(x, z) }

  def coalesce[A](ll: List[List[A]]): List[A] = 
    foldRight(ll, Nil: List[A]){ 
      (acc, l) =>
        append(acc, l)
    }
   
  def add1(l: List[Int]): List[Int] = 
    foldRight(l, Nil: List[Int])((x, acc) => Cons(x+1,acc))

  def doublesToStrings(l: List[Double]): List[String] = 
    foldRight(l, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  def map[A,B](l: List[A])(f: (A) => B): List[B] = 
    //foldRight(l, Nil: List[B]){ (a, acc) => Cons(f(a), acc) }
    foldRightWithFoldLeft(l, Nil: List[B]){ (a, acc) => Cons(f(a), acc) }

  def filter[A](l: List[A])(f: (A) => Boolean): List[A] = 
    foldRightWithFoldLeft(l, Nil: List[A]){ (a, acc) => if (f(a)) Cons(a, acc) else acc }

  def flatMap[A,B](l: List[A])(f: (A) => List[B]): List[B] = 
    coalesce(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: (A) => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def sumLists(l: List[Int], r: List[Int]): List[Int] = {
    def go(ll: List[Int], rr: List[Int], s: List[Int]): List[Int] = 
      (ll, rr) match {
        case (Nil, _) => append(reverse(s), rr)
        case (_, Nil) => append(reverse(s), ll)
        case (Cons(lh, lt), Cons(rh, rt)) => go(lt, rt, Cons(lh+rh, s))
      }
    go(l, r, Nil)
  }
   
  def pairwise[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = {
    def go(ll: List[A], rr: List[B], s: List[C]): List[C] = 
      (ll, rr) match {
        case (Nil, _) => reverse(s)
        case (_, Nil) => reverse(s)
        case (Cons(lh, lt), Cons(rh, rt)) => go(lt, rt, Cons(f(lh,rh), s))
      }
    go(l, r, Nil)
  }

  def hasSubseq[A](l: List[A], seq: List[A]): Boolean = {
    def go(ll: List[A], sseq: List[A]): Boolean = {
      (ll, sseq) match {
        case (Nil, Nil) => true
        case (_, Nil)   => true
        case (Nil, _)   => false
        case (Cons(lh, lt), Cons(sh, st)) => 
          if (lh == sh) go(lt, st)
          else go(lt, seq)
      }
    }
    go(l, seq)
  }

}
