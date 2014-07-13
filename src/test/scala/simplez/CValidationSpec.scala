package simplez

import org.specs2.mutable._


class CValidationSpec extends Specification {

  "A CValidation (Collection Validation" should {
    "behave like an Applicative" in {
      import simplez._
      import std.list._
      import std.string._
      import CValidation._

      type MyVal[A] = CValidation[List[String], A]

      val good : MyVal[Int] = CRight(3)
      val bad : MyVal[Int] = CLeft(List("error"))
      val F : Applicative[MyVal] = implicitly[Applicative[MyVal]]

      def helper(a : MyVal[Int], b: MyVal[Int]) = {
        F.apply2(a,b){ _ + _ }
      }

      "keep going with good results" in {
        helper(good, good) should beEqualTo(CRight(6))
      }

      "keep bad results" in {
        helper(bad, good) should beEqualTo(bad)
      }

      "keep bad results" in {
        helper(good, bad) should beEqualTo(bad)
      }

      "accumulate bad results" in {
        val result = helper(bad, bad)
        result should beEqualTo(CLeft(List("error","error")))
      }

    }
  }

}
