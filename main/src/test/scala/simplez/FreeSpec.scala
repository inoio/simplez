package simplez

import org.specs2.mutable._

import id._

class FreeSpec extends Specification {

  "The Free Monad" should {

    val nat: (Id ~> Id) = new NaturalTransformation[Id, Id] {
      override def apply[A](F: Id[A]): Id[A] = F
    }

    def faculty(i: Long): Free[Id, Long] = {
      if (i >= 1)
        for {
          rec <- Bind[Id, Long, Long](i, i => faculty(i - 1))
        } yield { i * rec }
      else
        Return(1)
    }

    "be able to calculate the faculty" in {

      "for 2!" in {
        faculty(2).foldMap(nat) should beEqualTo(2)
      }
      "for 3!" in {
        faculty(3).foldMap(nat) should beEqualTo(6)
      }
      "for 4!" in {
        faculty(4).foldMap(nat) should beEqualTo(24)
      }
      "for 10!" in {
        faculty(10).foldMap(nat) should beEqualTo(3628800)
      }
    }
  }

}
