package simplez

import simplez._
import simplez.std.future._
import simplez.std.option._
import simplez.std.list._
import org.specs2.mutable._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._

class FunctorSpec extends Specification {

  val C = Functor[Future].compose[Option]
  val P = Functor[List].product[Option]

  "A functor" should {
    "be composable" in {
      C.map(Future.successful { Option(2) })(_ * 3) must beSome(6).await
      C.map(Future.successful { None: Option[Int] })(_ * 3) must beNone.await
    }

    """be "productable"""" in {
      P.map((1 :: 2 :: 3 :: Nil, Some(3)))(_ * 3) must beEqualTo((3 :: 6 :: 9 :: Nil, Some(9)))
    }
  }
}