package simplez

package object std {

  object int {
    implicit val intInstances = new Monoid[Int] {
      def mzero = 0

      def mappend(a: Int, b: Int) = a + b
    }
  }

  object string {
    implicit val stringInstances = new Monoid[String] {
      override def mzero: String = ""

      override def mappend(a: String, b: String): String = a + b
    }
  }

  object list {
    implicit def listInstances[A: Monoid] = new Monoid[List[A]] with Foldable[List] {
      def mzero: List[A] = List.empty

      def mappend(a: List[A], b: List[A]) = a ++ b

      def foldMap[A, B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B = {
        fa.foldLeft(F.mzero) { case (start, elem) => F.mappend(start, f(elem))}
      }
    }

  }

  object option {
    implicit def optionInstances[A: Monoid] = new Monoid[Option[A]] {
      def mzero: Option[A] = None

      def mappend(a: Option[A], b: Option[A]): Option[A] = {
        (a, b) match {
          case (Some(a1), Some(b1)) => Some(Monoid[A].mappend(a1, b1))
          case (Some(_), _) => a
          case (_, Some(_)) => b
          case _ => mzero
        }
      }
    }

    implicit val optionNT: NaturalTransformation[Option, List] =
      new NaturalTransformation[Option, List] {
        def apply[A](F: Option[A]): List[A] = F.toList
      }
  }

}