package simplez.examples
import simplez._
import simplez.std.list._
import simplez.syntax._
import simplez.syntax.ApplicativeSyntax

object TreeExample extends App {

  import BinTree._
  val tree: BinTree[Int] = Bin(Bin(Leaf(1), Leaf(2)), Leaf(5))
  println(s"Tree: $tree")
  println(s"map _ * 3: ${tree.map(_ * 3)}")

  def sum(tree: BinTree[Int], initial: Int): Int = Foldable[BinTree].foldRight(tree, 0)((value, initial) => value + initial)

  println(s"sum : ${sum(tree, 0)}")
  println(s"contents : ${tree.contents()}")
  println(s"count : ${tree.count()}")
  println(s"shape: ${tree.shape()}")
  println(s"decompose: ${tree.decompose()}")
}

sealed trait BinTree[A]
case class Leaf[A](a: A) extends BinTree[A]
case class Bin[A](left: BinTree[A], right: BinTree[A]) extends BinTree[A]

case object BinTree {

  implicit val applicativeInstances: Applicative[BinTree] with Traverse[BinTree] = new Applicative[BinTree] with Traverse[BinTree] {

    override def map[A, B](fa: BinTree[A])(f: A => B): BinTree[B] = {
      fa match {
        case Leaf(a) => Leaf(f(a))
        case Bin(left, right) => Bin(map(left)(f), map(right)(f))
      }
    }

    def pure[A](a: => A): BinTree[A] = Leaf(a)

    def ap[A, B](t: => BinTree[A])(f: => BinTree[A => B]): BinTree[B] = {
      t match {
        case Leaf(a) => map(f)(g => g(a))
        case Bin(left, right) => Bin(ap(left)(f), ap(right)(f))
      }
    }

    override def traverse[G[_]: Applicative, A, B](fa: BinTree[A])(f: A => G[B]): G[BinTree[B]] = {
      val G = Applicative[G]
      fa match {
        case Leaf(a) => f(a).map(b => Leaf(b))
        case Bin(left, right) =>
          (traverse(left)(f) |@| traverse(right)(f)) { case (left, right) => Bin(left, right) }
      }
    }

    /**
     * Map each element of the structure to a [[Monoid]], and combine the
     * results.
     */
    override def foldMap[A, B](fa: BinTree[A])(f: (A) => B)(implicit F: Monoid[B]): B = {
      fa match {
        case Leaf(a) => f(a)
        case Bin(left, right) => foldMap(left)(f) append foldMap(right)(f)
      }
    }

    override def foldRight[A, B](fa: BinTree[A], z: => B)(f: (A, B) => B): B = {
      fa match {
        case Leaf(a) => f(a, z)
        case Bin(left, right) => foldRight(left, foldRight(right, z)(f))(f)
      }
    }
  }
}