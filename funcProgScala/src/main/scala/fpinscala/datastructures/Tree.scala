package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = 
    //t match {
    //  case Leaf(_) => 1
    //  case Branch(l, r) => 
    //    1 + size(l) + size(r)
    //}
    fold(t)(_ => 1)((lc, rc) => 1 + lc + rc)

  def maxIntValue(t: Tree[Int]): Int =  
    //t match {
    //  case Leaf(n) => n
    //  case Branch(l, r) => 
    //    maxIntValue(l).max(maxIntValue(r))
    //}
    fold(t)((n) => n)((ln, rn) => ln.max(rn))

  def depth[A](t: Tree[A]): Int = 
    //t match {
    //   case Leaf(_) => 0
    //   case Branch(l, r) =>  
    //     depth(l).max(depth(r)) + 1
    //}
    fold(t)(_ => 0)((ld, rd) => ld.max(rd) + 1)

  def map[A,B](t: Tree[A])(f: (A) => B): Tree[B] = 
    //t match {
    //  case Leaf(a) => Leaf(f(a))
    //  case Branch(l, r) => 
    //    Branch(map(l)(f), map(r)(f))
    //}
    fold(t)( { a => Leaf(f(a)): Tree[B] } )( (lb, rb) => Branch(lb, rb) )
    
  def fold[A,B](t: Tree[A])(l: (A) => B)(b: (B,B) => B): B = 
    t match {
      case leaf: Leaf[A] => l(leaf.value)
      case branch: Branch[A] => b(fold(branch.left)(l)(b), fold(branch.right)(l)(b))
    }
}
