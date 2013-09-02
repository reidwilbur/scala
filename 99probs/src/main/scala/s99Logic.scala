package ninetynineprobs {
  case class Logic(val v: Boolean) {
    def and(b: Logic): Logic = {
      (this, b) match {
        case (Logic(true), Logic(true)) => Logic(true)
        case _ => Logic(false)
      }
    }

    def or(b: Logic): Logic = {
      (this, b) match {
        case (Logic(true), _) => Logic(true)
        case (_, Logic(true)) => Logic(true)
        case _ => Logic(false)
      }
    }
  }

  object S99Logic {
  
    implicit def boolean2Logic(b: Boolean) = Logic(b)
    implicit def logic2Boolean(l: Logic) = l.v

    def and(a: Boolean, b: Boolean): Boolean =
      (a, b) match {
        case (true, true) => true
        case (_, _) => false
      }

    def or(a: Boolean, b: Boolean): Boolean =
      (a, b) match {
        case (true, _) => true
        case (_, true) => true
        case (_, _) => false
      }

    def not(a: Boolean): Boolean =
      a match {
        case true => false
        case false => true
      }

    def nand(a: Boolean, b: Boolean): Boolean =
      not(and(a,b))

    def nor(a: Boolean, b: Boolean): Boolean =
      not(or(a,b))

    def xor(a: Boolean, b: Boolean): Boolean =
      (a, b) match {
        case (true, false) => true
        case (false, true) => true
        case (_, _) => false
      }

    def equ(a: Boolean, b: Boolean): Boolean =
      not(xor(a,b))

    def impl(a: Boolean, b: Boolean): Boolean =
      or(not(a), b)

    def table2(f: (Boolean, Boolean) => Boolean): List[String] = {
      import collection.mutable.ListBuffer
      val lb = new ListBuffer[String]
      lb += "A     B     result"
      for(
        a <- List(false, true);
        b <- List(false, true)
      ) lb += ("%-5s %-5s %s".format(a, b, f(a,b)))
      lb.toList
    }
  }
}
