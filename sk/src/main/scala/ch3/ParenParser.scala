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
  
  object LP {
    def unapply(s: String): Option[String] = {
       if (s.startsWith("(")) Some(s.tail) else None
    }
  }
  
  object RP {
    def unapply(s: String): Option[String] = {
       if (s.startsWith(")")) Some(s.tail) else None
    }
  }
  
  def checkParens(input: String): Either[Success, Error] = {
    def check(parenStr: String, stack: Stack[Paren], idx: Int): Stack[Paren] = {
        parenStr match {
          case "" =>
            stack
            
  	      case LP(tail) =>
  	        check(tail, stack.push(LP(idx)), idx+1)
  	          
  	      case RP(tail) =>
  	        stack.headOption match {
              case None =>
                stack.push(RP(idx))
                
  	          case Some(_: LP) =>
  	            check(tail, stack.pop, idx+1)
  	            
  	          case Some(m) =>
  	            throw new IllegalStateException(s"Head of stack is $m which should not be possible.")
  	        }
  	          
  	      case s =>
  	        check(s.tail, stack, idx+1)
	      }
    }
    val stack = check(input, Stack(), 0)
    if (stack.isEmpty) Left(Success()) else Right(Error(stack.head.idx)) 
  }
}