package simplez.examples.tictactoe

import org.specs2.mutable._
import scalaz._
import Scalaz._

class TicTacToeSpec extends Specification {

  " A board" should {

    val p1 = Player1.some

    "be transposable" in {
      val notOccupied: List[Option[Player]] = List.fill(3)(None)
      val fields: List[List[Option[Player]]] = List(List(p1, p1, p1), notOccupied, notOccupied)
      val fieldsT = fields.transpose
      val expectedResult = List(notOccupied.updated(0, p1), notOccupied.updated(0, p1), notOccupied.updated(0, p1))
      fieldsT must beEqualTo(expectedResult)
    }

    "be transposable in the diagonal /" in {
      // 0 0 1
      // 0 1 0
      // 1 0 0

      val notOccupied: List[Option[Player]] = List.fill(3)(None)
      val fields: List[List[Option[Player]]] = List(notOccupied.updated(2, p1), notOccupied.updated(1, p1), notOccupied.updated(0, p1))
      val fieldsT = fields.transpose
      val expectedResult = fields
      fieldsT must beEqualTo(expectedResult)
    }

    "be transposable in the diagonal \\" in {
      // 0 0 1
      // 0 1 0
      // 1 0 0

      val notOccupied: List[Option[Player]] = List.fill(3)(None)
      val fields: List[List[Option[Player]]] = List(notOccupied.updated(0, p1), notOccupied.updated(1, p1), notOccupied.updated(2, p1))
      val fieldsT = fields.transpose
      val expectedResult = fields
      fieldsT must beEqualTo(expectedResult)
    }
  }
}