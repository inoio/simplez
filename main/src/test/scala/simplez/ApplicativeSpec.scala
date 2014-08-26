package simplez

import org.specs2.mutable._

import std.option._
import syntax._

class ApplicativeSpec extends Specification {

  "An applicative builder" should {

    "work for options (Some)" in {
      (Option(3) |@| Option(4) |@| Option(5)) { _ + _ + _ } should beSome(12)
    }

    "work for options (None)" in {
      (Option(3) |@| (None: Option[Int]) |@| Option(5)) { _ + _ + _ } should beNone
    }

    "work for mixed types" in {
      def test(a: Int, b: Double): Double = a + b
      (Option(3) |@| Option(4.0)) { test } should beSome(7.0)
    }
  }

}
