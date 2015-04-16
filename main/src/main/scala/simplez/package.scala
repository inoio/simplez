
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
    trait ConstApplicativeT[M] extends Applicative[Lambda[a => Const[M, a]]] {
      implicit def M: Monoid[M]
      override def map[A, B](fa: Const[M, A])(f: A => B): Const[M, B] = Const[M, B](fa.m)
      def pure[A](a: => A): Const[M, A] = Const[M, A](M.zero)
      def ap[A, B](fa: => Const[M, A])(f: => Const[M, A => B]): Const[M, B] = Const[M, B](M.append(f.m, fa.m))
    }

    implicit def ConstApplicative[M](implicit ev: Monoid[M]) = new ConstApplicativeT[M] {
      implicit def M: Monoid[M] = ev
    }
  }

  object coproduct {
    def coproductFunctor[F[_], G[_]](implicit F: Functor[F], G: Functor[G]) = new Functor[Coproduct[F, G, ?]] {
      def map[A, B](fa: Coproduct[F, G, A])(f: A => B): Coproduct[F, G, B] = {
        fa.value match {
          case Left(a) => new Coproduct[F, G, B](Left[F[B], G[B]](F.map(a)(f)))
          case Right(a) => new Coproduct[F, G, B](Right[F[B], G[B]](G.map(a)(f)))
        }
      }
    }

    implicit def leftInjectInstance[F[_], G[_]]: Inject[F, Coproduct[F, G, ?]] =
      new Inject[F, Coproduct[F, G, ?]] {
        def inj[A](fa: F[A]): Coproduct[F, G, A] = Coproduct.injl(fa)
      }

    //    implicit def rightInjectInstance[F[_], G[_], H[_]](implicit I: Inject[F, G]): Inject[F, Coproduct[H, G, ?]] =
    //      new Inject[F, Coproduct[H, G, ?]] {
    //        def inj[A](fa: F[A]): Coproduct[H, G, A] = Coproduct.injr(I.inj(fa))
    //      }

    implicit def rightInjectInstance[F[_], G[_]]: Inject[G, Coproduct[F, G, ?]] =
      new Inject[G, Coproduct[F, G, ?]] {
        def inj[A](fa: G[A]): Coproduct[F, G, A] = Coproduct.injr(fa)
      }
  }

}
