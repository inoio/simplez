package scalatypes

import scala.language.higherKinds
import scala.language.implicitConversions

trait Semigroup[A] {
  def mappend(a: A, b: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def mzero: A
}

object Monoid {
  def apply[A](implicit F: Monoid[A]): Monoid[A] = F
}

trait Functor[F[_]] {
  def map[A, B](F: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]) = F
}

trait Applicative[F[_]] {
  def ap[A, B](F: F[A])(f: F[A => B]): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](F: F[A])(f: A => F[B]): F[B]

  /**
   *  Implementation of `map` in terms of `flatMap`
   *
   */
  def map[A, B](F: F[A])(f: A => B): F[B] = {
    flatMap(F)(a => pure(f(a)))
  }
}

object Monad {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](F: F[A]): G[A]
}

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Return(a) => f(a)
      case Bind(fx, g) =>
        Bind(fx, g andThen (_ flatMap f))
    }

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Return(f(a)))

  def foldMap[G[_]: Monad](f: F ~> G): G[A] =
    this match {
      case Return(a) => Monad[G].pure(a)
      case Bind(fx, g) =>
        Monad[G].flatMap(f(fx)) { a =>
          g(a).foldMap(f)
        }
    }
}

case class Return[F[_], A](a: A)
  extends Free[F, A]

case class Bind[F[_], I, A](
  a: F[I],
  f: I => Free[F, A]) extends Free[F, A]

object NaturalTransformation {
  def apply[F[_], G[_]](implicit NT: NaturalTransformation[F, G]): NaturalTransformation[F, G] = NT
  implicit def reify[F[_], G[_], A](NT: F ~> G): F[A] => G[A] = { f => NT(f) }
}

trait Kleisli[F[_], A, B] {
  import Kleisli._
  def run(a: A): F[B]

  def andThen[C](k: Kleisli[F, B, C])(implicit b: Monad[F]): Kleisli[F, A, C] =
    kleisli((a: A) => b.flatMap(this.run(a))(k.run _))

  def >=>[C](k: Kleisli[F, B, C])(implicit b: Monad[F]): Kleisli[F, A, C] = this andThen k
  
  def >==>[C](f: B => F[C])(implicit b: Monad[F]) = this andThen kleisli(f)

  def compose[C](k: Kleisli[F, C, A])(implicit b: Monad[F]): Kleisli[F, C, B] = {
    k >=> this
  }
  
  def <=<[C](k: Kleisli[F, C, A])(implicit b: Monad[F]): Kleisli[F, C, B] = this compose k
  
  def <==<[C](f: C => F[A])(implicit b: Monad[F]): Kleisli[F, C, B] = this compose kleisli(f)
 
  def map[C](f: B => C)(implicit G: Functor[F]): Kleisli[F, A, C] = kleisli {
    (a: A) =>
      val b = this.run(a)
      G.map(b)(f)
  }

  def flatMap[C](f: B => F[C])(implicit G: Monad[F]): Kleisli[F, A, C] = kleisli {
    (a: A) =>
      val b = this.run(a)
      G.flatMap(b)(f)
  }

}

object Kleisli {
  def kleisli[F[_], A, B](f: A => F[B]): Kleisli[F, A, B] = new Kleisli[F, A, B] {
    def run(a: A): F[B] = f(a)
  }
}

trait State[S, A] {
  def run(s: S): (S, A)
  def map[B](f: A => B): State[S, B] = State[S, B] {
    s =>
      val (s1, a) = run(s)
      (s1, f(a))
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] {
    s =>
      val (s1, a) = run(s)
      val b = f(a)
      b.run(s1)
  }
}

object State {
  def apply[S, A](f: S => (S, A)) = new State[S, A] {
    def run(s: S) = f(s)
  }
}
