package simplez.examples
import simplez._
import simplez.std.list._
import simplez.std.anyVal._
import simplez.syntax._
import simplez.syntax.ApplicativeSyntax

object TreeExample extends App {

  import BinTree._
  val tree: BinTree[Int] = Bin(Bin(Leaf(1), Leaf(2)), Leaf(5))
  val fnTree: BinTree[Int => Int] = Bin(Bin(Leaf(_ + 1), Leaf(_ - 1000)), Leaf(_ => 0))

  val questionMark = Applicative[BinTree].ap(tree)(fnTree)
  println(questionMark)
  println(s"Tree: $tree")
  println(s"map _ * 3: ${tree.map(_ * 3)}")

  def sum(tree: BinTree[Int], initial: Int): Int = Foldable[BinTree].foldRight(tree, 0)((value, initial) => value + initial)

  val stateMonad = State.stateMonad[Int]
  import stateMonad._
  val (count, newTree) = tree.collectS(i => for { count <- get; _ <- put(count + 1) } yield ())(i => i + 1).run(0)

  val label: State[Int, Int] = for { label <- get; _ <- put(label + 1) } yield (label)
  val (_, tree2) = tree.disperseS(label, (i: Int) => (l: Int) => (l.toString, i)).run(100)

  println(s"sum : ${sum(tree, 0)}")
  println(s"contents : ${tree.contents()}")
  println(s"count : ${tree.count()}")
  println(s"shape: ${tree.shape()}")
  println(s"decompose: ${tree.decompose()}")
  println(s"collect: $count, $newTree")
  println(s"label: $tree2")

  // Filter elements with value 2 or 3 and count how many we have
  val elementCount = Monoid[Int].applicative
  // preserve these elements
  val lister = Monoid[List[Int]].applicative
  val app = elementCount.product[Lambda[a => List[Int]]](lister)

  def twoOrThree(i: Int) = if (i == 2 || i == 3) 1 else 0
  val (filteredCount, result) = app.traverse(tree)(i => (twoOrThree(i), if (twoOrThree(i) == 1) List(i) else List()))
  println(s"filtered count $filteredCount, result : $result")

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