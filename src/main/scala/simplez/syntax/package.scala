package simplez

import scala.language.{higherKinds, implicitConversions}

package object syntax {

  trait MonoidSyntax[A] {
    def self: A

    def F: Monoid[A]

    /**
     * alias for mappend.
     */
    def |+|(b: A): A = mappend(b)

    def mappend(b: A): A = F.append(self, b)

    def mzero(): A = F.mzero
  }

  implicit def ToMonoidOps[A: Monoid](a: A): MonoidSyntax[A] = new MonoidSyntax[A] {
    def self: A = a

    def F: Monoid[A] = implicitly[Monoid[A]]
  }

  trait FunctorSyntax[F[_], A] {
    def self: F[A]

    def F: Functor[F]

    def map[B](f: A => B): F[B] = F.map(self)(f)
  }

  implicit def ToFunctorOps[F[_] : Functor, A](a: F[A]): FunctorSyntax[F, A] = new FunctorSyntax[F, A] {
    def self = a

    def F: Functor[F] = implicitly[Functor[F]]
  }

  trait MonadSyntax[F[_], A] {
    def self: F[A]

    def F: Monad[F]

    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(self)(f)
  }

  implicit def ToMonadOps[F[_] : Monad, A](a: F[A]): MonadSyntax[F, A] = new MonadSyntax[F, A] {
    def self = a

    def F: Monad[F] = implicitly[Monad[F]]
  }


  trait WriterSyntax[W, A] {
    def self: A

    def set(w: W): Writer[W, A] = Writer(w -> self)
  }

  implicit def ToWriterOps[W, A](a: A) = new WriterSyntax[W, A] {
    def self: A = a
  }

  implicit def writerToMonad[W, A](w: Writer[W, A])(implicit W: Monoid[W]) = new Monad[({type λ[α] = Writer[W, α]})#λ] {
    override def flatMap[A, B](F: Writer[W, A])(f: (A) => Writer[W, B]): Writer[W, B] = F.flatMap(f)

    override def pure[A](a: A): Writer[W, A] = Writer(W.mzero -> a)
  }

  implicit def ToApplicativeOps[F[_] : Applicative,A](a: F[A]) = new ApplicativeSyntax[F,A] {
    val self = a
  }

  trait ApplicativeSyntax[F[_],A] {
    def self : F[A]

    def |@|[B](b1 : F[B]) = new ApplicativeBuilder[F,A,B] {
      val a = self
      val b = b1
    }
  }

  trait ApplicativeBuilder[M[_], A, B] {
    val a: M[A]
    val b: M[B]

    def apply[C](f: (A, B) => C)(implicit ap: Applicative[M]): M[C] = ap.apply2(a, b)(f)

    def tupled(implicit ap: Applicative[M]): M[(A, B)] = apply(Tuple2.apply)

    def |@|[C](cc: M[C]) = new ApplicativeBuilder3[C] {
      val c = cc
    }

    sealed trait ApplicativeBuilder3[C] {
      val c: M[C]

      def apply[D](f: (A, B, C) => D)(implicit ap: Applicative[M]): M[D] = ap.apply3(a, b, c)(f)

      def tupled(implicit ap: Applicative[M]): M[(A, B, C)] = apply(Tuple3.apply)

    }

  }

}