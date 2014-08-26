package simplez

import org.specs2.ScalaCheck
import org.specs2.mutable._

class MonoidSpec extends Specification with ScalaCheck {

  implicit val doubleMonoid = new Monoid[Double] {
    /**
     * the identity element.
     * @group( " b a s e " )
     */
    override def zero: Double = 0.0

    /**
     * The associative binary function.
     * @group( " b a s e " )
     */
    override def append(a: Double, b: Double): Double = a + b
  }

  "A monoid " should {
    "not work for Doubles due to IEEE imprecision" in {
      check((a: Double, b: Double) => Monoid[Double].append(a, b) - a - b == Monoid[Double].zero)
    }

    "work for Ints" in {
      import std.anyVal._
      check((a: Int, b: Int) => Monoid[Int].append(a, b) - a - b == Monoid[Int].zero)
    }
  }

}
