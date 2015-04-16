package simplez.examples
import scalaz._
package object tictactoe {
  /**
   * Phantom Types to describe the Board state.
   */
  sealed trait BoardState
  trait Empty extends BoardState
  trait InProgress extends BoardState
  trait Finished extends BoardState

  type Field = Option[Player \/ Player]
  type EitherBoard[S] = Status[Finished] \/ S
  type TicTacToeState[S1 <: BoardState, S2 <: BoardState, A] = IndexedStateT[EitherBoard, Status[S1], Status[S2], A]

  type Rows = List[List[Field]]
}