package simplez

import org.specs2.mutable._

class ListSpec extends Specification {

  import simplez.std.list._

  val list = 1 :: 2 :: 3 :: Nil

  "A List" should {
    "provide a map function" in {
      val Functor = implicitly[Functor[List]]
      Functor.map(list)(_ + 1) must beEqualTo(List(2, 3, 4))
    }

    "provide a flatMap function" in {
      val Monad = implicitly[Monad[List]]
      Monad.flatMap(list)((a: Int) => List(-a, a)) must beEqualTo(List(-1, 1, -2, 2, -3, 3))
    }

    "have a monoid instance" in {
      import simplez.std.int._
      val Monoid = implicitly[Monoid[List[Int]]]

      "with an mzero" in {
        Monoid.mzero must beEqualTo(List())
      }

      "with an append function" in {
        Monoid.append(list, list) must beEqualTo(List(1, 2, 3, 1, 2, 3))
      }
    }

    "have a foldable instance" in {
      "and can fold over strings" in {
        import simplez.std.string._
        // TODO why don't I get an instance via val F = implicitly[Foldable[List]]
        val result: String = Foldable[List].foldMap(list)(_.toString)
        result must beEqualTo("123")
      }

      "and can sum a list of ints" in {
        import simplez.std.int._
        Foldable[List].sum(list) must beEqualTo(6)
      }
    }
    "have an applicative instance" in {
      val APP = implicitly[Applicative[List]]
      "and can compute values with functions in a context" in {
        val neg = (a: Int) => -a
        val multBy2 = (a: Int) => 2 * a

        APP.ap(list)(List(neg, multBy2)) must beEqualTo(List(-1, -2, -3, 2, 4, 6))
      }
    }

    "have a traverse instance" in {
      val T = implicitly[Traverse[List]]
      import scala.concurrent.ExecutionContext.Implicits._
        import scala.concurrent._
        import scala.concurrent.duration._
        import std.future._
        val list = List(Future.successful(1), Future.successful(2))
      "and can sequence over a list of futures" in {
        
        val result = Await.result(T.sequence(list), Duration.Inf)
        result should beEqualTo(List(1,2))
      }
    }
  }
}
