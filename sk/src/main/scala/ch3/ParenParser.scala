package ch3

import scala.collection.immutable.Stack

object ParenParser {
  case class Success()
  
  case class Error(Location: Int)
  
  abstract class Paren {
    def idx: Int
  }
  case class LP(idx: Int) extends Paren
  case class RP(idx: Int) extends Paren
  
  def checkParens(input: String): Either[Success, Error] = {
    def check(itr: Iterator[Char], stack: Stack[Paren], idx: Int): Stack[Paren] = {
      if (!itr.hasNext)
        stack
      else {  
        itr.next match {
	      case '(' =>
	        check(itr, stack.push(LP(idx)), idx+1)
	          
	      case ')' =>
	        stack.headOption match {
	          case Some(LP(_)) =>
	            check(itr, stack.pop, idx+1)
	          case _ =>
	            check(itr, stack.push(RP(idx)), idx+1)
	        }
	          
	      case _ =>
	        check(itr, stack, idx+1)
	    }
      }
    }
    val stack = check(input.iterator, Stack(), 0)
    if (stack.isEmpty) Left(Success()) else Right(Error(stack.head.idx)) 
  }
}