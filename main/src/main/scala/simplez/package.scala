
package object simplez {

  type Id[A] = A

  type Reader[A, B] = Kleisli[Id, A, B]

  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  type ~~>[F[_[_]], G[_[_]]] = NaturalTransformation2[F, G]

  object id {
    implicit val idInstance = new Monad[Id] {
      def pure[A](a: => A): Id[A] = a

      def flatMap[A, B](F: Id[A])(f: A => Id[B]): Id[B] = f(F)
    }

    implicit val identityTransformation = new NaturalTransformation[Id, Id] {
      def apply[A](a: Id[A]) = a
    }
  }

  object const {
    trait ConstApplicativeT[M] extends Applicative[Const[M, ?]] {
      implicit def M: Monoid[M]
      override def map[A, B](fa: Const[M, A])(f: A => B): Const[M, B] = Const[M, B](fa.m)
      def pure[A](a: => A): Const[M, A] = Const[M, A](M.zero)
      def ap[A, B](fa: => Const[M, A])(f: => Const[M, A => B]): Const[M, B] = Const[M, B](M.append(f.m, fa.m))
    }

    implicit def ConstApplicative[M](implicit ev: Monoid[M]) = new ConstApplicativeT[M] {
      implicit def M: Monoid[M] = ev
    }
  }

}
