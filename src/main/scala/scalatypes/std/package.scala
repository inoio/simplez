package scalatypes

package object std {
  object int {
    implicit val intMonoid = new Monoid[Int] {
      def mzero = 0
      def mappend(a: Int, b: Int) = a + b
    }
  }

  object list {
    implicit def listMonoid[A: Monoid] = new Monoid[List[A]] {
      def mzero: List[A] = List.empty
      def mappend(a: List[A], b: List[A]) = a ++ b
    }

  }

  object option {
    implicit def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
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

    val x = Option(3)
    val result: List[Int] = NaturalTransformation[Option, List].apply(x)

    val L = List(Some(3), None, Some(4))
    val z = L.map(NaturalTransformation[Option, List])

  }
}