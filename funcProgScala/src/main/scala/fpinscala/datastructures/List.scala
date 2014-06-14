package fpinscala.datastructures

sealed trait LList[+A] // `List` data type, parameterized on a type, `A`
case object NNil extends LList[Nothing] // A `List` data constructor representing the empty list
case class CCons[+A](head: A, tail: LList[A]) extends LList[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `LList[A]`, which may be `NNil` or another `CCons`.

object LList { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: LList[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case NNil => 0 // The sum of the empty list is 0.
    case CCons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: LList[Double]): Double = ds match {
    case NNil => 1.0
    case CCons(0.0, _) => 0.0
    case CCons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): LList[A] = // Variadic function syntax
    if (as.isEmpty) NNil
    else CCons(as.head, apply(as.tail: _*))

  def append[A](a1: LList[A], a2: LList[A]): LList[A] =
    a1 match {
      case NNil => a2
      case CCons(h,t) => CCons(h, append(t, a2))
    }

  def foldRight[A,B](as: LList[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case NNil => z
      case CCons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: LList[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: LList[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](as: LList[A]): LList[A] = 
    as match {
      case NNil => NNil
      case CCons(_, tail) => tail
    }

  def setHead[A](h: A, as: LList[A]): LList[A] =
    CCons(h, tail(as))

  def drop[A](l: LList[A], n: Int): LList[A] = 
    l match {
      case NNil => NNil
      case _ if n == 0 => l
      case CCons(_, tail) => drop(tail, n-1)
    }

  def dropWhile[A](l: LList[A])(f: A => Boolean): LList[A] = 
    l match {
      case NNil => NNil
      case CCons(h, tail) if f(h) => dropWhile(tail)(f)
      case _ => l
    }

  def init[A](l: LList[A]): LList[A] = {
    def itr(from: LList[A], acc: LList[A]): LList[A] = {
      from match {
        case NNil => NNil
        case CCons(h, NNil) => acc
        case CCons(h, tail) => itr(tail, append(acc, CCons(h, NNil)))
      }
    }
    itr(l, NNil)
  }

  def length[A](l: LList[A]): Int = 
    foldRight(l, 0)((x: A, len: Int) => len+1)

  def foldLeft[A,B](l: LList[A], z: B)(f: (B, A) => B): B = 
    l match {
      case NNil => z
      case CCons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumFoldLeft(ns: LList[Int]): Int = 
    foldLeft(ns, 0)( (z, x) => z + x )

  def prodFoldLeft(ns: LList[Double]): Double = 
    foldLeft(ns, 1.0)( (z, x) => z * x )

  def lengthFoldLeft[A](l: LList[A]): Int = 
    foldLeft(l, 0)( (z, x) => z + 1 )

  def reverse[A](l: LList[A]): LList[A] = 
    foldLeft(l, NNil: LList[A])( (z, xs) => CCons(xs, z) )

  def foldRightWithFoldLeft[A,B](l: LList[A], z: B)(f: (A,B) => B): B = 
    foldLeft(reverse(l), z){(b, a) => f(a, b)}

  def appendWithFoldRight[A](l: LList[A], r: LList[A]): LList[A] = 
    foldRight(l, r){ (x, z) => CCons(x, z) }

  def coalesce[A](ll: LList[LList[A]]): LList[A] = 
    foldRight(ll, NNil: LList[A]){ 
      (acc, l) =>
        append(acc, l)
    }
   
  def add1(l: LList[Int]): LList[Int] = 
    foldRight(l, NNil: LList[Int])((x, acc) => CCons(x+1,acc))

  def doublesToStrings(l: LList[Double]): LList[String] = 
    foldRight(l, NNil: LList[String])((d, acc) => CCons(d.toString, acc))

  def map[A,B](l: LList[A])(f: (A) => B): LList[B] = 
    //foldRight(l, NNil: LList[B]){ (a, acc) => CCons(f(a), acc) }
    foldRightWithFoldLeft(l, NNil: LList[B]){ (a, acc) => CCons(f(a), acc) }

  def filter[A](l: LList[A])(f: (A) => Boolean): LList[A] = 
    foldRightWithFoldLeft(l, NNil: LList[A]){ (a, acc) => if (f(a)) CCons(a, acc) else acc }

  def flatMap[A,B](l: LList[A])(f: (A) => LList[B]): LList[B] = 
    coalesce(map(l)(f))

  def filterViaFlatMap[A](l: LList[A])(f: (A) => Boolean): LList[A] =
    flatMap(l)(x => if (f(x)) LList(x) else NNil)

  def sumLists(l: LList[Int], r: LList[Int]): LList[Int] = {
    def go(ll: LList[Int], rr: LList[Int], s: LList[Int]): LList[Int] = 
      (ll, rr) match {
        case (NNil, _) => append(reverse(s), rr)
        case (_, NNil) => append(reverse(s), ll)
        case (CCons(lh, lt), CCons(rh, rt)) => go(lt, rt, CCons(lh+rh, s))
      }
    go(l, r, NNil)
  }
   
  def pairwise[A,B,C](l: LList[A], r: LList[B])(f: (A,B) => C): LList[C] = {
    def go(ll: LList[A], rr: LList[B], s: LList[C]): LList[C] = 
      (ll, rr) match {
        case (NNil, _) => reverse(s)
        case (_, NNil) => reverse(s)
        case (CCons(lh, lt), CCons(rh, rt)) => go(lt, rt, CCons(f(lh,rh), s))
      }
    go(l, r, NNil)
  }

  def hasSubseq[A](l: LList[A], seq: LList[A]): Boolean = {
    def go(ll: LList[A], sseq: LList[A]): Boolean = {
      (ll, sseq) match {
        case (NNil, NNil) => true
        case (_, NNil)   => true
        case (NNil, _)   => false
        case (CCons(lh, lt), CCons(sh, st)) => 
          if (lh == sh) go(lt, st)
          else go(lt, seq)
      }
    }
    go(l, seq)
  }

}
