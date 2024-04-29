package spinalextras.lib.misc


case class Rational(x: Long, y: Long) {

  // require is used to enforce a precondition on the caller
  require(y != 0, "denominator must be non-zero")

  // define a greatest common divisor method we can use to simplify rationals
  private def gcd(a: Long, b: Long): Long = Math.abs(if (b == 0) a else gcd(b, a % b))
  //private def gcd(a: Long, b: Long): Long = BigLong(a).gcd(BigLong(b)).toLong//Math.abs(if (b == 0) a else gcd(b, a % b))
  lazy val g = gcd(x, y)

  lazy val numer = x / g
  lazy val denom = y / g

  // define a second constructor
  def this(x: Long) = this(x, 1)

  // define methods on this class
  def add(r: Rational): Rational =
    new Rational(numer * r.denom + r.numer * denom, denom * r.denom)
  def add(r: Long): Rational = add(new Rational(r, 1))

  def +(r: Rational): Rational = add(r)
  def +(r: Long): Rational = add(Rational(r, 1))

  // negation
  def neg = new Rational(-numer, denom)
  def unary_- : Rational = neg

  def sub(r: Rational): Rational = add(r.neg)
  def sub(r: Long): Rational = add(Rational(r, 1).neg)

  def -(r: Rational): Rational = sub(r)
  def -(r: Long): Rational = sub(r)
  def abs : Rational = Rational(numer.abs, denom.abs)

  def mult(r: Rational) =
    new Rational(numer * r.numer, denom * r.denom)

  def *(r: Rational): Rational = mult(r)

  def *(r: Long): Rational = new Rational(numer * r, denom)

  def div(r: Rational) =
    new Rational(numer * r.denom, denom * r.numer)

  def /(r: Rational): Rational = div(r)
  def /(r: Long): Rational = new Rational(numer, denom * r)

  def less(r: Rational): Boolean = numer * r.denom < r.numer * denom

  def <(r: Rational): Boolean = less(r)

  def more(r: Rational): Boolean = numer * r.denom > r.numer * denom

  def >(r: Rational): Boolean = more(r)

  def max(r: Rational): Rational = if (less(r)) r else this

  def min(r: Rational): Rational = if (more(r)) r else this

  def inv: Rational = new Rational(denom, numer)
  def unary_/ : Rational = inv
  def toDouble : Double = numer / denom.toDouble
  def toBigDecimal : BigDecimal = BigDecimal(numer) / denom
  override
  def toString: String = numer + "/" + denom
}
