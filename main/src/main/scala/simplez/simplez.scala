package simplez

import scala.Left
import scala.Right
import scala.language.higherKinds
import scala.language.implicitConversions

import std.list.listInstance0
import std.option.optionInstances

trait Category[=>:[_, _]] {
  self =>
  def id[A]: A =>: A
  def compose[A, B, C](g: B =>: C, f: A =>: B): A =>: C

  final def semigroup[A]: Semigroup[A =>: A] = new Semigroup[=>:[A, A]] {
    def append(a: A =>: A, b: => A =>: A) = self.compose(a, b)
  }

  final def monoid[A]: Monoid[A =>: A] = new Monoid[A =>: A] {
    def append(a: A =>: A, b: => A =>: A) = self.compose(a, b)
    def zero = id
  }
}

object Category {
  def apply[F[_, _]](implicit C: Category[F]) = C
}

/**
 * A Semigroup defines an associative binary function.
 *
 * {{{
 *  Semigroup[Int].mappend(3,4)
 * }}}
 *
 * === Laws ===
 * Associativity:  `a append (b append c) = (a append b) append c`
 *
 * @tparam A the type of the semigroup
 * @see [[http://en.wikipedia.org/wiki/Semigroup]]
 */
trait Semigroup[A] {
  /**
   * The associative binary function.
   * @group("base")
   */
  def append(a: A, b: => A): A
}

object Semigroup {
  def apply[A](implicit F: Semigroup[A]): Semigroup[A] = F
}

/**
 * A Monoid is a special [[Semigroup]] together with an identity element call mzero.
 *
 * === Law ===
 * Identity : mzero append a = a
 *
 * @tparam F the type of the monoid.
 * @see [[http://en.wikipedia.org/wiki/Monoid]]
 */
trait Monoid[F] extends Semigroup[F] {
  self =>
  /**
   * the identity element.
   * @group("base")
   */
  def zero: F

  def applicative: Applicative[λ[α => F]] = new Applicative[λ[α => F]] {
    // mapping just returns ourselves
    override def map[A, B](fa: F)(f: A => B): F = fa
    // putting any value into this Applicative will put the Monoid.zero in it
    def pure[A](a: => A): F = self.zero
    // Applying this Applicative combines each value with the append function.
    def ap[A, B](fa: => F)(f: => F): F = self.append(f, fa)
  }

  final def category: Category[λ[(α, β) => F]] = new Category[λ[(α, β) => F]] {
    def compose[A, B, C](g: F, f: F) = append(g, f)
    def id[A] = zero
  }
}

object Monoid {
  def apply[A](implicit F: Monoid[A]): Monoid[A] = F
}

/**
 * A functor is a structure which defines a mapping from F[A] to F[B].
 * Strictly speaking this a covariant functor.
 *
 * @tparam F a type constructor.
 * @see [[http://en.wikipedia.org/wiki/Functor]]
 */
trait Functor[F[_]] {
  self =>
  /**
   * the mapping function.
   * {{{
   * 	val listString = Functor[List].map(listInt){ (a:Int) => a.toString }
   * }}}
   *
   * === Laws ===
   * Identity: `F[A].map(x => x) = F[A]`
   * Composition: `F[A].map((b => c) compose (a => v)) = F[A].map(a => b).map(b=>c)`
   */
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /**
   *  The product of two Functors F[_] and G[_] is a Functor over (F[_],G[_]).
   *
   */
  def product[G[_]](implicit G: Functor[G]): Functor[λ[α => (F[α], G[α])]] = new Functor[λ[α => (F[α], G[α])]] {
    def map[A, B](pA: (F[A], G[A]))(f: A => B): (F[B], G[B]) = {
      pA match {
        case (left, right) => (self.map(left)(f), G.map(right)(f))
      }
    }
  }

  def compose[G[_]](implicit G: Functor[G]): Functor[λ[α => F[G[α]]]] = new Functor[λ[α => F[G[α]]]] {
    def map[A, B](fgA: F[G[A]])(f: A => B): F[G[B]] = self.map(fgA)(gA => G.map(gA)(f))
  }

  def as[A, B](fa: F[A])(b: => B): F[B] = map(fa)(_ => b)

  def void[A](fa: F[A]): F[Unit] = as(fa)(())
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]) = F
}

trait ContravariantFunctor[F[_]] {
  /**
   *
   * @group("base")
   */
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object ContravariantFunctor {
  def apply[F[_]](implicit F: ContravariantFunctor[F]) = F
}

/**
 *
 * @tparam F a type constructor.
 *
 * @see [[simplez.syntax.ApplicativeBuilder]] for the famous `|@|` (Admiral Akbhar) operator.
 */
trait Applicative[F[_]] extends Functor[F] with GenApApplyFunctions[F] {
  self =>

  def pure[A](a: => A): F[A]

  /**
   * execute a function f with a single parameter within a context F within that context fa : F[A].
   */
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

  /**
   * override map with ap of an Applicative.
   * As we have the means to put anything in our F now via pure the implementation looks like
   *  {{{
   *    ap(fa)(pure(f))
   *  }}}
   */
  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))

  def traverse[A, G[_], B](value: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] =
    G.traverse(value)(f)(this)

  def sequence[A, G[_]: Traverse](as: G[F[A]]): F[G[A]] =
    traverse(as)(a => a)

  def product[G[_]](implicit G: Applicative[G]): Applicative[λ[α => (F[α], G[α])]] = new Applicative[λ[α => (F[α], G[α])]] {
    def pure[A](a: => A): (F[A], G[A]) = (self.pure(a), G.pure(a))
    def ap[A, B](fgA: => (F[A], G[A]))(f: => (F[A => B], G[A => B])): (F[B], G[B]) = (self.ap(fgA._1)(f._1), G.ap(fgA._2)(f._2))
  }

  def compose[G[_]](implicit G: Applicative[G]): Applicative[λ[α => F[G[α]]]] = new Applicative[λ[α => F[G[α]]]] {
    /**
     *
     */
    def pure[A](a: => A): F[G[A]] = self.pure(G.pure(a))

    /**
     *
     */
    def ap[A, B](fgA: => F[G[A]])(f: => F[G[A => B]]): F[G[B]] = {
      self.apply2(f, fgA)((ff, ga) => G.ap(ga)(ff))
    }
  }
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}

/**
 * A Monad specialises a [[Functor]].
 *
 * @tparam F[_] a type constructor
 * @see [[http://ncatlab.org/nlab/show/monad+%28in+computer+science%29]]
 */
trait Monad[F[_]] extends Applicative[F] {
  self =>

  def flatMap[A, B](F: F[A])(f: A => F[B]): F[B]

  /**
   * Implementation of `map` in terms of `flatMap`
   *
   */
  override def map[A, B](F: F[A])(f: A => B): F[B] = {
    flatMap(F)(a => pure(f(a)))
  }

  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = {
    flatMap(fa)(a => flatMap(f)(g => pure(g(a))))
    //    for {
    //      a <- fa
    //      g <- f
    //    } yield { pure (g(a))}
  }

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  /**
   * alias for `join`.
   */
  def flatten[A](ffa: F[F[A]]): F[A] = join(ffa)
}

object Monad {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F
}

trait Foldable[F[_]] {
  /**
   * Map each element of the structure to a [[Monoid]], and combine the
   * results.
   */
  def foldMap[A, B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, B) => B): B

  /**
   * @group("derived")
   *
   * Sum with the Monoid over the identity function.
   */
  def fold[A](fa: F[A])(implicit F: Monoid[A]): A = foldMap(fa)(a => a)
}

case object Foldable {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  /**
   * @group("base")
   */
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

  /**
   * Traverse with the identity function.
   * 	@group("derived")
   */
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(identity)

  // Monad is actually a bind, less strict...
  final def traverseM[A, G[_], B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: Monad[F]): G[F[B]] = G.map(G.traverse(fa)(f)(this))(F.flatten)

  def traverseS[S, G[_]: Applicative, A, B](fa: F[A])(f: A => State[S, G[B]]): State[S, G[F[B]]] = {
    import State._
    implicit val A = stateMonad[S].compose[G]
    State[S, G[F[B]]](s => {
      val st = traverse[λ[α => State[S, G[α]]], A, B](fa)(f)
      st.run(s)
    })
  }

  def traverseConst[M: Monoid, A](fa: F[A])(f: A => Const[M, Nothing]): Const[M, F[Nothing]] = {
    implicit val CApp: Applicative[Const[M, ?]] = const.ConstApplicative[M]
    traverse[Const[M, ?], A, Nothing](fa)(f)
  }

  def reduce[A, M: Monoid](fa: F[A])(reducer: A => M): M = {
    traverseConst(fa)(a => Const[M, Nothing](reducer(a))).m
  }

  def reduceConst[A, M: Monoid](fa: F[A])(m: M): M = reduce(fa)(_ => m)

  /**
   * @return a list of the contents of F[A].
   */
  def contents[A](fa: F[A]): List[A] = {
    import std.list._
    reduce(fa)((a: A) => List(a))
  }

  /**
   * Determine the "length" of the structure F[A].
   */
  def count[A](fa: F[A]): Int = {
    reduceConst(fa)(1)(std.anyVal.intInstances)
  }

  /**
   *  Discard all values of F[A].
   *  This is the same as the void function.
   *  @return F[Unit]
   */
  def shape[A](fa: F[A]): F[Unit] = map(fa)((a: A) => ())

  override def void[A](fa: F[A]): F[Unit] = shape(fa)

  def decompose[A](fa: F[A]): (F[Unit], List[A]) = {
    import id._
    import const._
    import std.list._
    val shape: A => Id[Unit] = a => (())
    val content: A => Const[List[A], Unit] = a => Const[List[A], Unit](List(a))
    val product: A => (Id[Unit], Const[List[A], Unit]) = a => (shape(a), content(a))

    implicit val productApp: Applicative[λ[α => (Id[α], Const[List[A], α])]] = Applicative[Id].product[Const[List[A], ?]]
    val result = traverse[λ[α => (Id[α], Const[List[A], α])], A, Unit](fa)(product)
    (result._1, result._2.m)
  }

  /**
   * Given a list of contents elements: List[B] reassemble a structure of F[A] (A =:= Unit)
   * to a structure Option[F[B]] considering that the list of elements needs to match the structure.
   */
  def reassemble[A, B](fa: F[A])(elements: List[B])(implicit ev: A =:= Unit): Option[F[B]] = {
    import syntax._
    import std.option._
    implicit val stateOptionApplicative: Applicative[λ[α => State[List[B], Option[α]]]] =
      State.stateMonad[List[B]].compose[Option]
    val takeHead: State[List[B], Option[B]] = State {
      s: List[B] =>
        s match {
          case Nil => (Nil, None)
          case x :: xs => (xs, Some(x))
        }
    }
    traverseS(fa)(a => takeHead).eval(elements)
  }

  def collect[G[_]: Applicative, A, B](fa: F[A])(f: A => G[Unit], g: A => B): G[F[B]] = {
    val G = implicitly[Applicative[G]]
    val applicationFn: A => G[B] = a => G.ap(f(a))(G.pure((u: Unit) => g(a)))
    self.traverse(fa)(applicationFn)
  }

  def collectS[S, A, B](fa: F[A])(f: A => State[S, Unit], g: A => B): State[S, F[B]] = {
    collect[λ[α => State[S, α]], A, B](fa)(f, g)
  }

  def disperse[G[_]: Applicative, A, B, C](fa: F[A])(fb: G[B], g: A => B => C): G[F[C]] = {
    val G = implicitly[Applicative[G]]
    val applicationFn: A => G[C] = a => G.ap(fb)(G.pure(g(a)))
    self.traverse(fa)(applicationFn)
  }

  def disperseS[S, A, C](fa: F[A])(fb: State[S, S], g: A => S => C): State[S, F[C]] = {
    disperse[λ[α => State[S, α]], A, S, C](fa)(fb, g)
  }
}

object Traverse {
  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F
}

/**
 * A natural transformation defines a kind of conversion between type constructors.
 *
 * @tparam F
 * @tparam G
 * @see [[http://en.wikipedia.org/wiki/Natural_transformation]]
 */
abstract class NaturalTransformation[F[_], G[_]] {
  self =>
  def apply[A](F: F[A]): G[A]

  def or[H[_]](hg: H ~> G): Coproduct[F, H, ?] ~> G = new NaturalTransformation[Coproduct[F, H, ?], G] {
    def apply[A](fh: Coproduct[F, H, A]): G[A] = fh.value match {
      case Left(f) => self.apply(f)
      case Right(h) => hg.apply(h)
    }
  }
}

object NaturalTransformation {
  def apply[F[_], G[_]](implicit NT: NaturalTransformation[F, G]): NaturalTransformation[F, G] = NT

  /**
   * defines an implicit conversion from a natural transformation to a function `F[A] => G[A]`
   */
  implicit def reify[F[_], G[_], A](NT: F ~> G): F[A] => G[A] = { f => NT(f) }
}

/**
 *  Higher kinded natural transformation
 *
 */
abstract class NaturalTransformation2[-F[_[_]], +G[_[_]]] {
  def apply[M[_]](F: F[M]): G[M]

  implicit def reify[F[_[_]], G[_[_]], M[_]](NT: F ~~> G): F[M] => G[M] = { f => NT(f) }
}

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Return(a) => f(a)
      case Bind(fx, g) =>
        Bind(fx, g andThen ((free: Free[F, A]) => free flatMap f))
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

  /** alias for foldMap. */
  def runM[G[_]: Monad](f: F ~> G): G[A] = foldMap(f)
}

final case class Return[F[_], A](a: A) extends Free[F, A]

final case class Bind[F[_], I, A](a: F[I], f: I => Free[F, A]) extends Free[F, A]

object Free {
  def liftF[F[_], A](fa: => F[A])(implicit F: Functor[F]): Free[F, A] = Bind(fa, (a: A) => Return[F, A](a))
}

class Coproduct[F[_], G[_], A](val value: Either[F[A], G[A]])

object Coproduct {
  def injl[F[_], G[_], A](value: F[A]) = new Coproduct[F, G, A](Left(value))
  def injr[F[_], G[_], A](value: G[A]) = new Coproduct[F, G, A](Right(value))

  // def apply[C <: Coproduct[_,_,_], T](t: T)(implicit inj: Inject[C, T]): C = inj.inj(t)
  def apply[I[_], F[_], G[_], A](i: I[A])(implicit inj: Inject[I, Coproduct[F, G, ?]]) = inj.inj(i)

}

trait Inject[F[_], G[_]] {
  def inj[A](fa: F[A]): G[A]
}

/**
 *
 */
trait Kleisli[F[_], A, B] {

  import simplez.Kleisli._

  def run(a: A): F[B]

  /** `andThen` two Kleisli's. */
  def andThen[C](k: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] =
    kleisli((a: A) => M.flatMap(this.run(a))(k.run _))

  /** alias for andThen. */
  def >=>[C](k: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] = this andThen k

  def >==>[C](f: B => F[C])(implicit ev: Monad[F]) = this andThen kleisli(f)

  /**
   * `compose` two Kleisli's.
   */
  def compose[C](k: Kleisli[F, C, A])(implicit ev: Monad[F]): Kleisli[F, C, B] = {
    k >=> this
  }

  def <=<[C](k: Kleisli[F, C, A])(implicit ev: Monad[F]): Kleisli[F, C, B] = this compose k

  def <==<[C](f: C => F[A])(implicit ev: Monad[F]): Kleisli[F, C, B] = this compose kleisli(f)

  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] = kleisli {
    (a: A) =>
      val b = this.run(a)
      F.map(b)(f)
  }

  def mapK[G[_], C](f: F[B] => G[C])(implicit ev: Functor[F]): Kleisli[G, A, C] = kleisli {
    a: A =>
      f(this.run(a))
  }

  def flatMap[C](f: B => Kleisli[F, A, C])(implicit M: Monad[F]): Kleisli[F, A, C] = kleisli {
    (r: A) =>
      val b = this.run(r)
      M.flatMap(b) { b: B => f(b).run(r) }
  }

  def flatMapK[C](f: B => F[C])(implicit M: Monad[F]): Kleisli[F, A, C] =
    kleisli(a => M.flatMap(run(a))(f))

  def local[AA](f: AA => A): Kleisli[F, AA, B] = kleisli(f andThen run)
}

object Kleisli {

  trait KleisliMonad[F[_], R] extends Monad[Kleisli[F, R, ?]] {
    implicit def F: Monad[F]
    override def pure[A](a: => A): Kleisli[F, R, A] = kleisli { _ => F.pure(a) }

    override def flatMap[A, B](fa: Kleisli[F, R, A])(f: A => Kleisli[F, R, B]): Kleisli[F, R, B] = fa.flatMap(f)
    override def ap[A, B](fa: => Kleisli[F, R, A])(f: => Kleisli[F, R, A => B]): Kleisli[F, R, B] =
      kleisli[F, R, B](r => F.ap(fa.run(r))(f.run(r)))
  }

  def kleisli[F[_], A, B](f: A => F[B]): Kleisli[F, A, B] = new Kleisli[F, A, B] {
    def run(a: A): F[B] = f(a)
  }

  implicit def kleisliMonad[T[_], R](implicit M: Monad[T]): KleisliMonad[T, R] = new KleisliMonad[T, R] {
    override implicit def F: Monad[T] = M
  }
}

object Reader {
  def apply[A, B](f: A => B): Reader[A, B] = Kleisli.kleisli[Id, A, B](f)

  implicit def readerMonad[X]: Monad[Reader[X, ?]] = new Monad[Reader[X, ?]] {
    override def flatMap[A, B](F: Reader[X, A])(f: (A) => Reader[X, B]): Reader[X, B] = Reader { x =>
      val a = F.run(x)
      f(a).run(x)
    }

    override def pure[A](a: => A): Reader[X, A] = Reader { x: X => a }
  }
}

trait State[S, A] {
  def run(s: S): (S, A)

  def runZero(implicit M: Monoid[S]): (S, A) = run(M.zero)

  def exec(initial: S): S = run(initial)._1

  def eval(initial: S): A = run(initial)._2

  def map[B](f: A => B): State[S, B] = State {
    s =>
      val (s1, a) = run(s)
      (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State {
    s =>
      val (s1, a) = run(s)
      val b = f(a)
      b.run(s1)
  }

  def get: State[S, S] = State {
    s =>
      val (s1, a) = run(s)
      (s1, s1)
  }

  def init = get

  def put(s: S): State[S, Unit] = State { _ => (s, ()) }

  def gets[T](f: S ⇒ T): State[S, T] = for { s <- get } yield (f(s))

  def modify(f: S ⇒ S): State[S, Unit] = for {
    s <- get
    _ <- put(f(s))
  } yield ()

}

object State {
  def apply[S, A](f: S => (S, A)) = new State[S, A] {
    def run(s: S) = f(s)
  }

  trait StateMonad[S] extends Monad[State[S, ?]] {
    override def flatMap[A, B](F: State[S, A])(f: (A) => State[S, B]): State[S, B] = F.flatMap(f)

    override def pure[A](a: => A): State[S, A] = State[S, A](s => (s, a))

    def get: State[S, S] = State { s => (s, s) }

    def init = get

    def put(s: S): State[S, Unit] = State { _ => (s, ()) }

    def gets[T](f: S ⇒ T): State[S, T] = for { s <- get } yield (f(s))

    def modify(f: S ⇒ S): State[S, Unit] = for {
      s <- get
      _ <- put(f(s))
    } yield ()

  }

  implicit def stateMonad[S]: StateMonad[S] = new StateMonad[S] {}
}

/**
 * A Writer keeps track of information on the right hand side of its (W,A) tuple, mapping over the A and appending
 * the W side where necessary.
 *
 * This implementation lacks the details and usefulness of the scalaz implementation as it omits the
 * F[_] typeconstructor for values of W.
 *
 *
 * {{{
 *   def addition(x1: Int, y1: Int) = for {
 * x <- x1.set(List(s"x =  $x1"))
 * y <- y1.set(List(s"y =  $y1"))
 * result <- (x + y).set(List(s"Adding values ${x + y}"))
 * } yield result
 *
 * val Writer((w,a)) = addition(10, 20)
 * }}}
 *
 * @constructor
 * Construct a new Writer with a initial tuple (W,A).
 *
 * @tparam W the "log" side
 * @tparam A the "value side"
 */
final case class Writer[W, A](run: (W, A)) {

  /**
   * Map over the A.
   */
  def map[B](f: A => B): Writer[W, B] = {
    val (w, a) = run
    Writer(w -> f(a))
  }

  /**
   * flatMap concatenating the two written sides.
   */
  def flatMap[B](f: A => Writer[W, B])(implicit s: Semigroup[W]): Writer[W, B] = {
    val (w, a) = run
    val (w1, b) = f(a).run
    Writer(s.append(w, w1) -> b)
  }

  /**
   * Return the written W side.
   */
  def written = run._1

  /**
   * Return the value A side.
   */
  def value = run._2

  /**
   * map over the written side.
   */
  def mapWritten[W2](f: W => W2)(implicit F: Functor[Id]): Writer[W2, A] = {
    val w2 = F.map(written)(f)
    Writer((w2 -> value))
  }

  /**
   * Map over the tuple (W,A) of the Writer.
   * Strange naming - it does not map of the value A! map does that.
   */
  def mapValue[X, B](f: ((W, A)) => (X, B))(implicit F: Functor[Id]): Writer[X, B] =
    Writer(F.map(run)(f))

  /**
   * Prepend a W to a writer.
   * {{{
   * "String" <++: writer
   * }}}
   */
  def <++:(w: => W)(implicit F: Functor[Id], W: Semigroup[W]): Writer[W, A] =
    mapWritten(W.append(w, _))

  /**
   * Append a W to a writer.
   * {{{
   * 		writer :++> "String"
   * }}}
   */
  def :++>(w: => W)(implicit F: Functor[Id], W: Semigroup[W]): Writer[W, A] =
    mapWritten(W.append(_, w))

  /**
   * Clear the written side with a Monoid.
   */
  def reset(implicit W: Monoid[W]): Writer[W, A] = {
    Writer(W.zero -> value)
  }
}

/**
 * A monad transformer which encapsulates the monad Option in any Monad F.
 *
 * E.g. a `Future[Option[A]]` can be encapsulated in a OptionT[Future, A].
 * Anytime you have the structure M[N[A] and you do not care about the outer N
 * at the moment, you can choose an NT Monad Transformer, e.g.
 * ListT[Option,A] if you have an Option[List[A]] or a
 * OptionT[Future, A] if you have a Future[Option[A]]
 *
 */
final case class OptionT[F[_], A](run: F[Option[A]]) {
  self =>

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    new OptionT[F, B](mapO((opt: Option[A]) => opt map f))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] = new OptionT[F, B](
    F.flatMap(self.run) {
      // partial functions: expected A => F[B]
      case None => F.pure(None)
      case Some(z) => f(z).run
    }
  )

  def isEmpty(implicit F: Functor[F]): F[Boolean] = mapO(_.isEmpty)

  def get(implicit F: Functor[F]): F[A] = mapO(_.get)

  /**
   * This works as an internal helper function to make it easier to rewrite the API of the underlying monad.
   * @param f the function you would like to invoke on the underlying monad
   * @param F for this to work we need an implicit functor instance
   * @tparam B the result type
   * @return the result in the outer monad.
   */
  private def mapO[B](f: Option[A] => B)(implicit F: Functor[F]): F[B] = F.map(run)(f)
}

case object OptionT {
  /**
   * This is essentially a function from the MonadTrans type class.
   * Considering you have a Future[A] but need a Future[Option[A]] aka
   * a OptionT[Future, A] you can lift the future into the monad of
   * the monad transformer.
   */
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
    OptionT[G, A](G.map(a)(a => Some(a)))
}

final case class ListT[F[A], A](run: F[List[A]]) {
  self =>
  def map[B](f: A => B)(implicit F: Functor[F]): ListT[F, B] =
    ListT[F, B](mapO((list: List[A]) => list map f))

  def flatMap[B](f: A => ListT[F, B])(implicit F: Monad[F]): ListT[F, B] = ListT[F, B](
    F.flatMap(self.run) {
      case Nil => F.pure(Nil)
      case nonEmpty => nonEmpty.map(f).reduce(_ ++ _).run
    }
  )

  def headOption(implicit F: Functor[F]): F[Option[A]] = mapO(_.headOption)

  def head(implicit F: Functor[F]): F[A] = mapO(_.head)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = mapO(_.isEmpty)

  def ++(bs: => ListT[F, A])(implicit F: Monad[F]): ListT[F, A] = new ListT(F.flatMap(run) { list1 =>
    F.map(bs.run) { list2 =>
      list1 ++ list2
    }
  })

  private def mapO[B](f: List[A] => B)(implicit F: Functor[F]) = F.map(run)(f)
}

case object ListT {
  /**
   *
   * @see [[OptionT.liftM]]
   */
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): ListT[G, A] = ListT[G, A](
    G.map(a)(a => List(a))
  )
}

sealed trait CValidation[A, B] {

  def ap[C](x: => CValidation[A, B => C])(implicit S: Semigroup[A]): CValidation[A, C] = {
    (this, x) match {
      case (CRight(a), CRight(f)) => CRight(f(a))
      case (CRight(a), CLeft(error)) => CLeft(error)
      case (CLeft(error), CRight(f)) => CLeft(error)
      case (CLeft(error1), CLeft(error2)) => CLeft(S.append(error1, error2))
    }
  }

  def map[C](f: B => C): CValidation[A, C] = {
    this match {
      case CLeft(error) => CLeft[A, C](error)
      case CRight(good) => CRight[A, C](f(good))
    }
  }

  def flatMap[C](f: (B) => CValidation[A, C]): CValidation[A, C] = {
    this match {
      case CLeft(error) => CLeft[A, C](error)
      case CRight(good) => f(good)
    }
  }

}

object CValidation {

  implicit def cvalidationInstances1[X](implicit SG: Semigroup[X]): Monad[CValidation[X, ?]] = new Monad[CValidation[X, ?]] {

    override def flatMap[A, B](F: CValidation[X, A])(f: (A) => CValidation[X, B]): CValidation[X, B] = {
      F.flatMap(f)
    }

    override def pure[A](a: => A): CValidation[X, A] = CRight(a)

    /**
     * It is required to overwrite ap as we need to keep collecting the results.
     */
    override def ap[A, B](F: => CValidation[X, A])(f: => CValidation[X, (A) => B]): CValidation[X, B] = F.ap(f)(SG)
  }

}
final case class CLeft[A, B](run: A) extends CValidation[A, B]

final case class CRight[A, B](run: B) extends CValidation[A, B]

case class Const[M, A](m: M)

