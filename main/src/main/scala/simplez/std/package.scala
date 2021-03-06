package simplez

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds
import scala.util.Either.RightProjection
import scala.util._

package object std {

  object anyVal {

    implicit val byteInstances = new Monoid[Byte] {
      def zero: Byte = 0

      def append(a: Byte, b: => Byte): Byte = (a + b).toByte
    }

    implicit val shortInstances = new Monoid[Short] {
      def zero: Short = 0

      def append(a: Short, b: => Short): Short = (a + b).toShort
    }

    implicit val intInstances = new Monoid[Int] {
      def zero: Int = 0

      def append(a: Int, b: => Int): Int = a + b
    }

    implicit val longInstances = new Monoid[Long] {
      def zero: Long = 0L

      def append(a: Long, b: => Long): Long = a + b
    }

    implicit val unitInstances = new Monoid[Unit] {
      def zero: Unit = ()
      def append(a: Unit, b: => Unit): Unit = ()
    }

    /**
     * conjunction.
     */
    implicit val boolInstances = new Monoid[Boolean] {
      def zero = true

      def append(a: Boolean, b: => Boolean) = a && b
    }
  }

  object string {
    implicit val stringInstances = new Monoid[String] {
      override def zero: String = ""

      override def append(a: String, b: => String): String = a + b

    }
  }

  object list {
    implicit def listInstance0[A] = new Monoid[List[A]] {
      override def zero: List[A] = List.empty[A]

      override def append(a: List[A], b: => List[A]): List[A] = a ++ b
    }

    implicit val listInstance1 = new Monad[List] with Foldable[List] with Traverse[List] {

      /**
       * Map each element of the structure to a [[Monoid]], and combine the\
       * results.
       */
      override def foldMap[A, B](fa: List[A])(f: (A) => B)(implicit F: Monoid[B]): B = {
        fa.foldLeft(F.zero) { case (a, b) => F.append(a, f(b)) }
      }

      override def traverse[G[_]: Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] = {
        val emptyListInG: G[List[B]] = Applicative[G].pure(List.empty[B])
        foldRight(fa, emptyListInG) { (a: A, fbs: G[List[B]]) => Applicative[G].apply2(f(a), fbs)(_ +: _) }
      }

      override def flatMap[A, B](F: List[A])(f: (A) => List[B]): List[B] = F.flatMap(f)

      override def foldRight[A, B](fa: List[A], z: => B)(f: (A, B) => B): B = fa match {
        case Nil => z
        case head :: tail =>
          f(head, foldRight(tail, z)(f))
      }

      override def pure[A](a: => A): List[A] = List(a)
    }

    implicit def listTInstance[F[_]](implicit M: Monad[F]) = new Monad[ListT[F, ?]] {
      override def flatMap[A, B](F: ListT[F, A])(f: (A) => ListT[F, B]): ListT[F, B] = F.flatMap(f)

      override def pure[A](a: => A): ListT[F, A] = ListT[F, A](M.pure(List(a)))
    }
  }

  implicit def mapInstances[A, B](implicit S: Semigroup[B]) = new Semigroup[Map[A, B]] {
    def append(a: Map[A, B], b: => Map[A, B]) = {
      val bInst = b
      bInst.foldLeft(a) { case (res, (k, v)) => res + (k -> a.get(k).map(v2 => S.append(v, v2)).getOrElse(v)) }
    }
  }

  object future {
    implicit def futureInstance(implicit ec: ExecutionContext) = new Monad[Future] {
      override def flatMap[A, B](F: Future[A])(f: (A) => Future[B]): Future[B] = F.flatMap(f)

      override def pure[A](a: => A): Future[A] = Future {
        a
      }(ec)

      override def ap[A, B](F: => Future[A])(f: => Future[A => B]): Future[B] = {
        for {
          fut <- F
          g <- f
        } yield { g(fut) }
      }

    }
  }

  object option {
    implicit def optionInstances0[A: Semigroup] = new Monoid[Option[A]] {
      def zero: Option[A] = None

      def append(a: Option[A], b: => Option[A]): Option[A] = {
        (a, b) match {
          case (Some(a1), Some(b1)) => Some(Semigroup[A].append(a1, b1))
          case (Some(_), _) => a
          case (_, Some(_)) => b
          case _ => zero
        }
      }
    }

    implicit def optionInstances = new Foldable[Option] with Applicative[Option] with Traverse[Option] with Monad[Option] {

      def foldMap[A, B](fa: Option[A])(f: A => B)(implicit M: Monoid[B]): B = fa.map(f).getOrElse(M.zero)

      override def foldRight[A, B](fa: Option[A], z: => B)(f: (A, B) => B): B = fa match {
        case None => z
        case Some(head) => f(head, z)
      }

      override def pure[A](a: => A): Option[A] = Some(a)

      /**
       * execute a function f with a single parameter within a context F within that context fa : F[A].
       */
      override def ap[A, B](fa: => Option[A])(f: => Option[(A) => B]): Option[B] = f match {
        case Some(f) => fa match {
          case Some(x) => Some(f(x))
          case None => None
        }
        case None => None
      }

      override def traverse[G[_]: Applicative, A, B](fa: Option[A])(f: (A) => G[B]): G[Option[B]] = {
        fa match {
          case None => Applicative[G].pure(None)
          case Some(a) => Applicative[G].map(f(a))(a => Some(a))
        }
      }

      override def flatMap[A, B](F: Option[A])(f: (A) => Option[B]): Option[B] = F.flatMap(f)

    }

    implicit val optionNT: NaturalTransformation[Option, List] =
      new NaturalTransformation[Option, List] {
        def apply[A](F: Option[A]): List[A] = F.toList
      }
  }

  object either {
    /**
     * right based either monad.
     *
     */
    implicit def eitherInstances[X] = new Monad[Either[X, ?]] {
      override def flatMap[A, B](F: Either[X, A])(f: (A) => Either[X, B]): Either[X, B] = F.right.flatMap(f)

      override def pure[A](a: => A): Either[X, A] = Right(a)

    }
    implicit def eitherSemigroup[A, B](implicit SA: Semigroup[A], SB: Semigroup[B]) = new Semigroup[Either[A, B]] {
      def append(a: Either[A, B], b: => Either[A, B]): Either[A, B] = {
        (a, b) match {
          case (Left(a), Left(b)) => Left(SA.append(a, b))
          case (Left(a), Right(_)) => Left(a)
          case (Right(a), Right(b)) => Right(SB.append(a, b))
          case (Right(_), Left(b)) => Left(b)
        }

      }
    }
  }

  object function {
    implicit val function1Instances = new Category[Function1] {
      def compose[A, B, C](g: B => C, f: A => B): A => C = g compose f
      def id[A] = a => a
    }

    implicit def functionMonoid[A] = new Monoid[A => A] {
      def append(a: A => A, b: => A => A): A => A = a andThen b
      def zero = a => a
    }

    implicit def functionMonoid[I, O](implicit M: Monoid[O]) = new Monoid[I => O] {
      def append(a: I => O, b: => I => O): I => O = x => M.append(a(x), b(x))
      def zero = a => M.zero
    }
  }
}