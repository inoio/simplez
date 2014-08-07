package simplez

import org.specs2.mutable._

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }

class ListTSpec extends Specification {

  "A ListT monad transformer" should {

    "work like a list" in {
      "in map/flatMap Style" in {
        import std.future._

        def listFn(x: Int): Future[List[Int]] = Future.successful(List(-x, x))

        val f: Future[List[Int]] = Future.successful {
          List(1, 2, 3)
        }

        val listT: ListT[Future, Int] = ListT(f).flatMap(x => ListT[Future, Int](listFn(x))).map(x => x + 1)
        Await.result(listT.run, Duration.Inf) should beEqualTo(List(0, 2, -1, 3, -2, 4))
      }

      "and for comprehensions" in {
        import std.future._

        val f: Future[List[Int]] = Future.successful {
          List(1, 2, 3)
        }

        val listT = for {
          elem <- ListT[Future, Int](f)
        } yield {
          elem + 1
        }
        Await.result(listT.run, Duration.Inf) should beEqualTo(List(2, 3, 4))
      }
    }

    "and have" in {
      "have a liftM function to lift a x:G[A] into a ListT[G,A]" in {
        import std.future._
        val x = Future.successful("Hello")

        Await.result(ListT.liftM(x).run, Duration.Inf) should beEqualTo(List("Hello"))
      }
    }

  }

}
