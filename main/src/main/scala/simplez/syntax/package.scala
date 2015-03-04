package simplez

import scala.language.{ higherKinds, implicitConversions }

/**
 * This package provides implicit functions, so that we can for example access
 * {{{
 *    val x : List[Int] = List(1,2,3)
 *    x.mappend(List(4,5,6)
 * }}} instead of
 * {{{
 *    Monoid[List].append(x, List(1,2,3)
 * }}}
 *
 */
package object syntax {

  trait SemigroupSyntax[A] {
    def self: A

    def F: Semigroup[A]

    def |+|(b: A): A = append(b)

    def append(b: A): A = F.append(self, b)

  }

  implicit def ToSemigroupOps[A: Semigroup](a: A): SemigroupSyntax[A] = new SemigroupSyntax[A] {
    def self: A = a

    def F: Semigroup[A] = implicitly[Semigroup[A]]
  }

  /**
   *
   * @tparam A
   */
  trait MonoidSyntax[A] {
    def self: A

    def F: Monoid[A]

    def mzero(): A = F.zero
  }

  /**
   *
   * @param a
   * @tparam A
   * @return
   */
  implicit def ToMonoidOps[A: Monoid](a: A): MonoidSyntax[A] = new MonoidSyntax[A] {
    def self: A = a

    def F: Monoid[A] = implicitly[Monoid[A]]
  }

  /**
   *
   * @tparam F
   * @tparam A
   */
  trait FunctorSyntax[F[_], A] {
    def self: F[A]

    def F: Functor[F]

    def map[B](f: A => B): F[B] = F.map(self)(f)

    def void: F[Unit] = F.void(self)

    def as[B](b: => B) = F.as(self)(b)
  }

  /**
   *
   * @param a
   * @tparam F
   * @tparam A
   * @return
   */
  implicit def ToFunctorOps[F[_]: Functor, A](a: F[A]): FunctorSyntax[F, A] = new FunctorSyntax[F, A] {
    def self = a

    def F: Functor[F] = implicitly[Functor[F]]
  }

  /**
   *
   * @tparam F
   * @tparam A
   */
  trait MonadSyntax[F[_], A] {
    def self: F[A]

    def F: Monad[F]

    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(self)(f)

    def pure[A](a: => A): F[A] = F.pure(a)
  }

  /**
   *
   * @param a
   * @tparam F
   * @tparam A
   * @return
   */
  implicit def ToMonadOps[F[_]: Monad, A](a: F[A]): MonadSyntax[F, A] = new MonadSyntax[F, A] {
    def self = a

    def F: Monad[F] = implicitly[Monad[F]]
  }

  /**
   *
   * @tparam W
   * @tparam A
   */
  trait WriterSyntax[W, A] {
    def self: A

    def set(w: W): Writer[W, A] = Writer(w -> self)
  }

  /**
   *
   * @param a
   * @tparam W
   * @tparam A
   * @return
   */
  implicit def ToWriterOps[W, A](a: A) = new WriterSyntax[W, A] {
    def self: A = a
  }

  trait FoldableSyntax[F[_], A] {
    def self: F[A]

    def F: Foldable[F]

    def foldRight[B](z: B)(f: (A, B) => B): B = F.foldRight(self, z)(f)

    def foldMap[B](f: A => B)(implicit ev: Monoid[B]): B = F.foldMap(self)(f)

    def fold(implicit ev: Monoid[A]): A = F.foldMap(self)(identity _)
  }

  implicit def ToFoldableOps[F[_]: Foldable, A](t: F[A]) = {
    def self: F[A] = t
    def F: Foldable[F] = implicitly[Foldable[F]]
  }

  /**
   *
   * @param w
   * @param W
   * @tparam W
   * @tparam A
   * @return
   */
  implicit def writerToMonad[W, A](w: Writer[W, A])(implicit W: Monoid[W]) = new Monad[({ type λ[α] = Writer[W, α] })#λ] {
    override def flatMap[A, B](F: Writer[W, A])(f: (A) => Writer[W, B]): Writer[W, B] = F.flatMap(f)

    override def pure[A](a: => A): Writer[W, A] = Writer(W.zero -> a)
  }

  /**
   *
   * @param a
   * @tparam F
   * @tparam A
   * @return
   */
  implicit def ToApplicativeOps[F[_]: Applicative, A](a: F[A]) = new ApplicativeSyntax[F, A] {
    val self = a
    val F = Applicative[F]
  }

  /**
   *
   * @tparam F
   * @tparam A
   */
  trait ApplicativeSyntax[F[_], A] {
    def self: F[A]
    def F: Applicative[F]

    final def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)

    /** Combine `self` and `fb` according to `Applicative[F]` with a function that discards the `A`s */
    final def *>[B](fb: F[B]): F[B] = F.apply2(self, fb)((_, b) => b)

    /** Combine `self` and `fb` according to `Applicative[F]` with a function that discards the `B`s */
    final def <*[B](fb: F[B]): F[A] = F.apply2(self, fb)((a, _) => a)

    def |@|[B](b1: F[B]) = new ApplicativeBuilder[F, A, B] {
      val a = self
      val b = b1
    }

  }

  /**
   *
   * @tparam F
   * @tparam A
   * @tparam B
   */
  trait ApplicativeBuilder[F[_], A, B] {
    val a: F[A]
    val b: F[B]

    def apply[C](f: (A, B) => C)(implicit ap: Applicative[F]): F[C] = ap.apply2(a, b)(f)

    def tupled(implicit ap: Applicative[F]): F[(A, B)] = apply(Tuple2.apply)

    def |@|[C](cc: F[C]) = new ApplicativeBuilder3[C] {
      val c = cc
    }

    sealed trait ApplicativeBuilder3[C] {
      val c: F[C]

      def apply[D](f: (A, B, C) => D)(implicit ap: Applicative[F]): F[D] = ap.apply3(a, b, c)(f)

      def tupled(implicit ap: Applicative[F]): F[(A, B, C)] = apply(Tuple3.apply)

      sealed trait ApplicativeBuilder4[D] {
        val d: F[D]

        def apply[E](f: (A, B, C, D) => E)(implicit ap: Applicative[F]): F[E] = ap.apply4(a, b, c, d)(f)

        def tupled(implicit ap: Applicative[F]): F[(A, B, C, D)] = apply(Tuple4.apply)

        def |@|[E](ee: F[E]) = new ApplicativeBuilder5[E] {
          val e = ee
        }

        sealed trait ApplicativeBuilder5[E] {
          val e: F[E]

          def apply[G](f: (A, B, C, D, E) => G)(implicit ap: Applicative[F]): F[G] = ap.apply5(a, b, c, d, e)(f)

          def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E)] = apply(Tuple5.apply)

          def |@|[G](gg: F[G]) = new ApplicativeBuilder6[G] {
            val g = gg
          }

          sealed trait ApplicativeBuilder6[G] {
            val g: F[G]

            def apply[H](f: (A, B, C, D, E, G) => H)(implicit ap: Applicative[F]): F[H] = ap.apply6(a, b, c, d, e, g)(f)

            def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G)] = apply(Tuple6.apply)

            def |@|[H](hh: F[H]) = new ApplicativeBuilder7[H] {
              val h = hh
            }

            sealed trait ApplicativeBuilder7[H] {
              val h: F[H]

              def apply[I](f: (A, B, C, D, E, G, H) => I)(implicit ap: Applicative[F]): F[I] = ap.apply7(a, b, c, d, e, g, h)(f)

              def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H)] = apply(Tuple7.apply)

              def |@|[I](ii: F[I]) = new ApplicativeBuilder8[I] {
                val i = ii
              }

              sealed trait ApplicativeBuilder8[I] {
                val i: F[I]

                def apply[J](f: (A, B, C, D, E, G, H, I) => J)(implicit ap: Applicative[F]): F[J] = ap.apply8(a, b, c, d, e, g, h, i)(f)

                def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I)] = apply(Tuple8.apply)

                def |@|[J](jj: F[J]) = new ApplicativeBuilder9[J] {
                  val j = jj
                }

                sealed trait ApplicativeBuilder9[J] {
                  val j: F[J]

                  def apply[K](f: (A, B, C, D, E, G, H, I, J) => K)(implicit ap: Applicative[F]): F[K] = ap.apply9(a, b, c, d, e, g, h, i, j)(f)

                  def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J)] = apply(Tuple9.apply)

                  def |@|[K](kk: F[K]) = new ApplicativeBuilder10[K] {
                    val k = kk
                  }

                  sealed trait ApplicativeBuilder10[K] {
                    val k: F[K]

                    def apply[L](f: (A, B, C, D, E, G, H, I, J, K) => L)(implicit ap: Applicative[F]): F[L] = ap.apply10(a, b, c, d, e, g, h, i, j, k)(f)

                    def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K)] = apply(Tuple10.apply)

                    def |@|[L](ll: F[L]) = new ApplicativeBuilder11[L] {
                      val l = ll
                    }

                    sealed trait ApplicativeBuilder11[L] {
                      val l: F[L]

                      def apply[M](f: (A, B, C, D, E, G, H, I, J, K, L) => M)(implicit ap: Applicative[F]): F[M] = ap.apply11(a, b, c, d, e, g, h, i, j, k, l)(f)

                      def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L)] = apply(Tuple11.apply)

                      def |@|[M](mm: F[M]) = new ApplicativeBuilder12[M] {
                        val m = mm
                      }

                      sealed trait ApplicativeBuilder12[M] {
                        val m: F[M]

                        def apply[N](f: (A, B, C, D, E, G, H, I, J, K, L, M) => N)(implicit ap: Applicative[F]): F[N] = ap.apply12(a, b, c, d, e, g, h, i, j, k, l, m)(f)

                        def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M)] = apply(Tuple12.apply)

                        def |@|[N](nn: F[N]) = new ApplicativeBuilder13[N] {
                          val n = nn
                        }

                        sealed trait ApplicativeBuilder13[N] {
                          val n: F[N]

                          def apply[O](f: (A, B, C, D, E, G, H, I, J, K, L, M, N) => O)(implicit ap: Applicative[F]): F[O] = ap.apply13(a, b, c, d, e, g, h, i, j, k, l, m, n)(f)

                          def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N)] = apply(Tuple13.apply)

                          def |@|[O](oo: F[O]) = new ApplicativeBuilder14[O] {
                            val o = oo
                          }

                          sealed trait ApplicativeBuilder14[O] {
                            val o: F[O]

                            def apply[P](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O) => P)(implicit ap: Applicative[F]): F[P] = ap.apply14(a, b, c, d, e, g, h, i, j, k, l, m, n, o)(f)

                            def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O)] = apply(Tuple14.apply)

                            def |@|[P](pp: F[P]) = new ApplicativeBuilder15[P] {
                              val p = pp
                            }

                            sealed trait ApplicativeBuilder15[P] {
                              val p: F[P]

                              def apply[Q](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P) => Q)(implicit ap: Applicative[F]): F[Q] = ap.apply15(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p)(f)

                              def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P)] = apply(Tuple15.apply)

                              def |@|[Q](qq: F[Q]) = new ApplicativeBuilder16[Q] {
                                val q = qq
                              }

                              sealed trait ApplicativeBuilder16[Q] {
                                val q: F[Q]

                                def apply[R](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q) => R)(implicit ap: Applicative[F]): F[R] = ap.apply16(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q)(f)

                                def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q)] = apply(Tuple16.apply)

                                def |@|[R](rr: F[R]) = new ApplicativeBuilder17[R] {
                                  val r = rr
                                }

                                sealed trait ApplicativeBuilder17[R] {
                                  val r: F[R]

                                  def apply[S](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R) => S)(implicit ap: Applicative[F]): F[S] = ap.apply17(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r)(f)

                                  def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R)] = apply(Tuple17.apply)

                                  def |@|[S](ss: F[S]) = new ApplicativeBuilder18[S] {
                                    val s = ss
                                  }

                                  sealed trait ApplicativeBuilder18[S] {
                                    val s: F[S]

                                    def apply[T](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T)(implicit ap: Applicative[F]): F[T] = ap.apply18(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r, s)(f)

                                    def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = apply(Tuple18.apply)

                                    def |@|[T](tt: F[T]) = new ApplicativeBuilder19[T] {
                                      val t = tt
                                    }

                                    sealed trait ApplicativeBuilder19[T] {
                                      val t: F[T]

                                      def apply[U](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U)(implicit ap: Applicative[F]): F[U] = ap.apply19(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(f)

                                      def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = apply(Tuple19.apply)

                                      def |@|[U](uu: F[U]) = new ApplicativeBuilder20[U] {
                                        val u = uu
                                      }

                                      sealed trait ApplicativeBuilder20[U] {
                                        val u: F[U]

                                        def apply[V](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V)(implicit ap: Applicative[F]): F[V] = ap.apply20(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(f)

                                        def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = apply(Tuple20.apply)

                                        def |@|[V](vv: F[V]) = new ApplicativeBuilder21[V] {
                                          val v = vv
                                        }

                                        sealed trait ApplicativeBuilder21[V] {
                                          val v: F[V]

                                          def apply[W](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W)(implicit ap: Applicative[F]): F[W] = ap.apply21(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)(f)

                                          def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = apply(Tuple21.apply)

                                          def |@|[W](ww: F[W]) = new ApplicativeBuilder22[W] {
                                            val w = ww
                                          }

                                          sealed trait ApplicativeBuilder22[W] {
                                            val w: F[W]

                                            def apply[X](f: (A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W) => X)(implicit ap: Applicative[F]): F[X] = ap.apply22(a, b, c, d, e, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w)(f)

                                            def tupled(implicit ap: Applicative[F]): F[(A, B, C, D, E, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W)] = apply(Tuple22.apply)

                                          }

                                        }

                                      }

                                    }

                                  }

                                }

                              }

                            }

                          }

                        }

                      }

                    }

                  }

                }

              }

            }

          }

        }

      }

    }

  }

}