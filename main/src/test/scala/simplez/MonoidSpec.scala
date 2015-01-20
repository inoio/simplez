package simplez

import org.specs2.ScalaCheck
import org.specs2.mutable._
import shapeless._
import shapeless.test.illTyped

class MonoidSpec extends Specification with ScalaCheck {

  "A monoid " should {
    "not exists for Doubles due to IEEE imprecision" in {
      illTyped {
        """
        implicitly[Monoid[Double]]
        """
      }
      ok
    }

    "work for Ints" in {
      import std.anyVal._
      check((a: Int, b: Int) => Monoid[Int].append(a, b) - a - b == Monoid[Int].zero)
    }
  }

}
