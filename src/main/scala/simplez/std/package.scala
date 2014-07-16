package simplez

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

package object std {

  object byte {
    implicit val byteInstances = new Monoid[Byte] {
      def mzero: Byte = 0

      def append(a: Byte, b: Byte): Byte = (a + b).toByte
    }
  }

  object short {
    implicit val shortInstances = new Monoid[Short] {
      def mzero: Short = 0

      def append(a: Short, b: Short): Short = (a + b).toShort
    }
  }

  object int {
    implicit val intInstances = new Monoid[Int] {
      def mzero: Int = 0

      def append(a: Int, b: Int): Int = a + b
    }
  }

  object long {
    implicit val longInstances = new Monoid[Long] {
      def mzero: Long = 0L

      def append(a: Long, b: Long): Long = a + b
    }
  }

  object string {
    implicit val stringInstances = new Monoid[String] {
      override def mzero: String = ""

      override def append(a: String, b: String): String = a + b
    }
  }

  object list {
    implicit def listInstance1[A] = new Monoid[List[A]] with Monad[List] with Foldable[List] with Traverse[List] {
      override def mzero: List[A] = List.empty[A]

      /**
       * Map each element of the structure to a [[Monoid]], and combine the\
       * results.
       */
      override def foldMap[A, B](fa: List[A])(f: (A) => B)(implicit F: Monoid[B]): B = {
        fa.foldLeft(F.mzero) { case (a, b) => F.append(a, f(b))}
      }

      override def traverse[G[_] : Applicative, A, B](fa: List[A])(f: (A) => G[B]): G[List[B]] =
        fa.foldRight(Applicative[G].pure(List[B]())) {
        (a, fbs) => Applicative[G].apply2(f(a), fbs)(_ :: _)
      }

      override def append(a: List[A], b: List[A]): List[A] = a ++ b
    
      override def flatMap[A, B](F: List[A])(f: (A) => List[B]): List[B] = F.flatMap(f)

      override def pure[A](a: A): List[A] = List(a)
    }

    implicit def listTInstance[F[_]](implicit M: Monad[F]) = new Monad[({type l[a] = ListT[F, a]})#l] {
      override def flatMap[A, B](F: ListT[F, A])(f: (A) => ListT[F, B]): ListT[F, B] = F.flatMap(f)

      override def pure[A](a: A): ListT[F, A] = ListT[F, A](M.pure(List(a)))
    }
  }

  object future {
    implicit def futureInstance(implicit ec: ExecutionContext) = new Monad[Future]  {
      override def flatMap[A, B](F: Future[A])(f: (A) => Future[B]): Future[B] = F.flatMap(f)

      override def pure[A](a: A): Future[A] = Future { a }(ec)
    }
  }

  object option {
    implicit def optionInstances[A: Monoid] = new Monoid[Option[A]] {
      def mzero: Option[A] = None

      def append(a: Option[A], b: Option[A]): Option[A] = {
        (a, b) match {
          case (Some(a1), Some(b1)) => Some(Monoid[A].append(a1, b1))
          case (Some(_), _) => a
          case (_, Some(_)) => b
          case _ => mzero
        }
      }
    }
    
    implicit def optionInstance1[A] = new Monad[Option] {
      override def flatMap[A, B](F: Option[A])(f: (A) => Option[B]): Option[B] = F.flatMap(f)

      override def pure[A](a: A): Option[A] = Some(a) : Option[A]
    }

    implicit val optionNT: NaturalTransformation[Option, List] =
      new NaturalTransformation[Option, List] {
        def apply[A](F: Option[A]): List[A] = F.toList
      }
  }

}