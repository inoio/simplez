package scalatypes

import scala.language.implicitConversions
import scala.language.higherKinds


/**
 * Example of using scaladoc wiki syntax. I use this example to make sure
 * [[https://wiki.scala-lang.org/display/SW/Syntax scaladoc syntax page]]
 * examples actually work. In particular, I could not get the wiki syntax lists
 * to work based on the documentation.
 *
 * This is another paragraph (note the empty line above) containing '''bold''',
 * ''italic'', `monospace`, __underline__, ^superscript^, and ,,subscript,,
 * words. This sentence uses the inline elements specified in the section
 * "Inline elements" on the syntax page that are sometimes different from the
 * example for _italic_, *bold*, +underline+, {{monspace}}, ^superscript^, and
 * ~subscript~. Why are there multiple ways of specifying the same format?
 * Apparently there aren't, the ones from the inline elements section do not
 * work.
 *
 * == Inline elements ==
 * This section contains a correct listing of inline elements. It is also a
 * handy example of an unordered list as well as escaping.
 *
 *  - '''Italic''': `''text''` becomes ''text''.
 *  - '''Bold''': `'''text'''` becomes '''text'''.
 *  - '''Underline''': `__text__` becomes __text__.
 *  - '''Monospace''': use backticks, I couldn't figure out how to escape
 *    other than sticking something in an inline monospace section, `text`.
 *  - '''Superscript''': `^text^` becomes ^text^.
 *  - '''Subscript''': `,,text,,` becomes ,,text,,.
 *  - '''Entity links''': `[[scala.collection.Seq]]` becomes
 *    [[scala.collection.Seq]]. As far as I know there is know way to link to
 *    external scaladoc so this is useless except for linking to other classes
 *    in the same build.
 *  - '''External links''': `[[http://scala-lang.org Scala web site]]` becomes
 *    [[http://scala-lang.org Scala web site]].
 *
 * == Block elements ==
 * Paragraphs should be obvious by now, just include a blank line. So lets move
 * to code blocks with a simple fibonacci example:
 *
 * {{{
 * def fib(n: Int) = if (n < 2) n else fib(n - 1) + fib(n - 2)
 * }}}
 *
 * Headings are pretty straightforward, lets show some examples:
 * =h1=
 * Note that the default style for h1 makes it some white color with a drop
 * shadow that is difficult to see in the main body of the documentation.
 * ==h2==
 * ===h3===
 * ====h4====
 * =====h5=====
 * ======h6======
 *
 * == Lists ==
 * There is an example unordered list for the inline elements. This example will
 * be more gratuitous and try the various list types that are supported. I must
 * be an idiot because I couldn't figure out how to make unordered lists work
 * without looking at the scaladoc source code. Now it seems rather obvious
 * from the instructions. The problem I had was the "`$` is the left margin"
 * bit. I kept trying to include a `$` in the code to now avail. The other
 * problems is that the first whitespace after the `*` is ignored. However,
 * I still contend that with a simple example it would have been obvious right
 * away, so here are some list examples that have been tested and actually
 * generate a list:
 *
 *  1. item one
 *  1. item two
 *    - sublist
 *    - next item
 *  1. now for broken sub-numbered list, the leading item must be one of
 *     `-`, `1.`, `I.`, `i.`, `A.`, or `a.`. And it must be followed by a space.
 *    1. one
 *    2. two
 *    3. three
 *  1. list types
 *    I. one
 *      i. one
 *      i. two
 *    I. two
 *      A. one
 *      A. two
 *    I. three
 *      a. one
 *      a. two
 *
 * I didn't see it mentioned on the document but you can also add a horizontal
 * rule with 4 dashes. See hr below:
 *
 * ----
 *
 * Ok now a brief look at supported javadoc tags. `@code` gets mapped to
 * inline monospace, e.g., {@code testing}. `@docRoot` and `@inheritDoc` are
 * mapped to empty strings. `@link`, `@linkplain`, and `@value` are also mapped
 * to inline monospace, e.g., {@link link}, {@linkplain linkplain},
 * {@value value}. Note it seems linkplain is confused with link. `@literal`
 * just dumps the value in without modification, e.g., {@literal some value
 * '''in a literal''' that __will__ get wiki formatting}.
 *
 * @author subnormal numbers
 * @see scala.collection.Seq
 */
package object syntax {

  trait MonoidSyntax[A] {
    def self: A
    def F: Monoid[A]
    
    /**
     * alias for mappend.
     */
    def |+|(b: A) : A = mappend(b)
    def mappend(b: A): A = F.mappend(self, b)
    def mzero() : A = F.mzero
  }
  
  implicit def ToMonoidOps[A : Monoid ](a : A) : MonoidSyntax[A] = new MonoidSyntax[A] {
    def self : A = a
    def F : Monoid[A] = implicitly[Monoid[A]]
  } 
  
  trait FunctorSyntax[F[_], A] {
    def self : F[A]
    def F : Functor[F]
    def map[B](f: A => B) : F[B] = F.map(self)(f)
  }
  
  implicit def ToFunctorOps[F[_] : Functor, A](a: F[A]) : FunctorSyntax[F, A] = new FunctorSyntax[F,A] {
    def self = a
    def F : Functor[F] = implicitly[Functor[F]]
  }
  
  trait MonadSyntax[F[_], A]{
    def self : F[A]
    def F : Monad[F]
    def flatMap[B](f: A => F[B]) : F[B] = F.flatMap(self)(f)
  }
  
  implicit def ToMonadOps[F[_] : Monad, A](a: F[A]) : MonadSyntax[F, A] = new MonadSyntax[F,A] {
    def self = a
    def F : Monad[F] = implicitly[Monad[F]]
  }

}