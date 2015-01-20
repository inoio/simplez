package simplez.examples

import simplez._
import syntax._
import std.list._
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.{ Applicative => PlayApplicative, Monoid => PlayMonoid, _ }

object PlayExample extends App {

  // transform any play monoid to a simplez monoid
  implicit object PlayMonoid2Monoid extends (PlayMonoid ~> Monoid) {
    def apply[A](playM: PlayMonoid[A]): Monoid[A] = new Monoid[A] {
      def zero: A = playM.identity
      def append(a: A, b: A): A = playM.append(a, b)
    }
  }

  // transform any concrete play monoid instance into the concrete simplez monoid instance
  // using the natural transformation ~>
  implicit def playMonoid2Monoid[A](implicit P: PlayMonoid[A], ev: PlayMonoid ~> Monoid): Monoid[A] = {
    ev(P)
  }

  val jsonList: List[JsArray] = (1 to 10).map(i => Json.arr(i)).toList

  val aggregate: JsArray = Foldable[List].fold(jsonList)
  println(s"Result of sum: $aggregate")

  // transform any play applicative into a simplez applicative
  implicit object PlayApplicative2Applicative extends (PlayApplicative ~~> Applicative) {
    def apply[M[_]](F: PlayApplicative[M]): Applicative[M] = new Applicative[M] {
      def pure[A](a: A) = F.pure(a)
      def ap[A, B](fa: => M[A])(f: => M[A => B]): M[B] = F.apply(f, fa)
    }
  }

  // transform any concrete play applicative instance into a simplez applicative instance
  // using a higher kinded "natural transformat" ~~>
  implicit def playApplicative2Applicative[F[_]](implicit P: PlayApplicative[F],
    ev: PlayApplicative ~~> Applicative): Applicative[F] = {
    ev(P)
  }

  case class ABC(a: Int, b: Int, c: Int)

  case object ABC {
    implicit val abcReads: Reads[ABC] = (
      (__ \ "a").read[Int] |@|
      (__ \ "b").read[Int] |@|
      (__ \ "c").read[Int]) { ABC.apply _ }
  }

}
