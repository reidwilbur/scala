#!/users/rwilbur/scala-2.10.2/bin/scala
!#

class Rational(n: Int, d: Int) {
  
  require(d != 0)
 
  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def this(n: Int) = this(n , 1)

  override def toString = numer+"/"+denom

  def + (that: Rational): Rational = 
    new Rational(
      numer*that.denom + that.numer*denom,
      denom*that.denom
    )

  def + (i: Int): Rational =
    new Rational(numer + i*denom, denom)

  def - (that:Rational): Rational =
    new Rational(
      numer*that.denom - that.numer*denom,
      denom*that.denom
    )

  def - (i:Int): Rational =
    new Rational(numer - i*denom, denom)

  def * (that: Rational): Rational =
    new Rational(numer*that.numer, denom*that.denom)

  def * (i: Int): Rational =
    new Rational(numer*i, denom)

  def / (that: Rational): Rational =
    new Rational(numer*that.denom, denom*that.numer)

  def / (i: Int): Rational =
    new Rational(numer, denom*i)

  private def gcd(a: Int, b: Int): Int = 
    if (b == 0) a else gcd(b, a % b)
}

implicit def initToRational(x: Int) = new Rational(x)

val oneHalf = new Rational(1,2)
val twoThirds = new Rational(2,3)
val oneThird = new Rational(1,3)

println("1/2+2/3="+(oneHalf + twoThirds))

println("1/2*2/3="+(oneHalf * twoThirds))

println("1/3+3="+(oneThird + 3))

println("1/3-3="+(oneThird - 3))

println("1/3 / 1/3="+(oneThird / oneThird))

println("1/2 / 2="+(oneHalf / 2))

println(new Rational(5))

println(new Rational(66,42))

println("2 * 1/2="+(2 * oneHalf))
