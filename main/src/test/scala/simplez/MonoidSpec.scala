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

    "forms a category" in {
      import std.list._
      import std.string._
      val M = Monoid[List[String]]
      val C = M.category
      "compose" in {
        check((a: List[String], b: List[String]) => M.append(a, b) == C.compose(a, b))
      }
      "id" in {
        M.zero == C.id
      }
    }
  }

}
