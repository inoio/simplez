package simplez

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import std.anyVal.intInstances

class MonoidSpec extends Specification with ScalaCheck {

  "A monoid " should {

    "work for Ints" in {
      import std.anyVal._
      check((a: Int, b: Int) => Monoid[Int].append(a, b) - a - b == Monoid[Int].zero)
    }
  }

}
