package simplez.examples.tictactoe

import scalaz._
import Scalaz._

sealed trait BoardState
trait Empty extends BoardState
trait InProgress extends BoardState
trait Finished extends BoardState

sealed trait Player
case object Player1 extends Player
case object Player2 extends Player

case object Player {
  implicit val enumInstance = new Enum[Player] {
    def pred(p: Player): Player = {
      p match {
        case Player1 => Player2
        case Player2 => Player1
      }
    }
    def succ(p: Player) = pred(p)

    def order(p1: Player, p2: Player): Ordering = {
      (p1, p2) match {
        case (Player1, Player2) => Ordering.LT
        case (Player2, Player1) => Ordering.GT
        case _ => Ordering.EQ
      }
    }
  }
}

/**
 * The game state.
 * @param playerUp indicates the player who is up to move.
 */
case class Status[S <: BoardState](board: Board[S], playerUp: Player)

case class Board[State <: BoardState] private (fields: List[Option[Player]]) {
  override def toString() = {
    val line = "\n-------\n"
    val boardAsStrings = for {
      field <- fields
    } yield {
      field match {
        case Some(Player1) => "X"
        case Some(Player2) => "O"
        case _ => " "
      }
    }
    boardAsStrings.sliding(3, 3).toList.map(row => row.mkString("|", "|", "|")).mkString(line, line, line)
  }
}

case object Board {
  /** create an empty board. */
  def apply(): Board[Empty] = Board[Empty](List.fill(9)(None))
  def asRows(board: Board[_]): List[List[Option[Player]]] = board.fields.sliding(3, 3).toList
  def asColumns(board: Board[_]): List[List[Option[Player]]] = asRows(board).transpose
  implicit def convertTupleToPos(t: (Int, Int)): Int = t._1 * 3 + t._2
}

case object TicTacToe {
  import Player._
  import Board._

  type EitherBoard[S] = Status[Finished] \/ S
  type TicTacToeState[S1 <: BoardState, S2 <: BoardState, A] = IndexedStateT[EitherBoard, Status[S1], Status[S2], A]

  object TicTacToeState {
    def apply[S1 <: BoardState, S2 <: BoardState, A](f: Status[S1] => EitherBoard[(Status[S2], A)]) = IndexedStateT[EitherBoard, Status[S1], Status[S2], A] { f }
  }

  private def inProgress(board: Board[Empty]): Board[InProgress] = board.copy()
  private def finish(board: Board[InProgress]): Board[Finished] = board.copy()

  /**
   * create an empty board.
   */
  def apply(): Status[Empty] = Status[Empty](Board(), Player1)

  /**
   * @return true if player is a winner of this game
   */
  private def computeWinner(board: Board[InProgress], player: Player): Boolean = {
    type Row = List[List[Option[Player]]]
    def diag(fields: List[Option[Player]]): List[Option[Player]] = List(fields((0 -> 0)), fields((1 -> 1)), fields(2 -> 2))
    val allRows: Row = asRows(board)
    val allColumns: Row = asColumns(board)
    val allDiags: Row = List(diag(allRows.flatten), diag(allColumns.flatten))
    val all: Row = allRows ++ allColumns ++ allDiags
    val F = Foldable[List].compose[Option]
    val aggregate: List[Int] = for {
      element <- all
    } yield {
      F.foldMap(element) {
        case `player` => 1
        case _ => 0
      }
    }
    aggregate.exists(_ == 3)
  }

  /**
   * start a game of Tic Tac Toe
   */
  def start(): TicTacToeState[Empty, InProgress, Unit] = TicTacToeState {
    status =>
      (Status[InProgress](board = inProgress(status.board), playerUp = Player1), ()).right
  }

  /**
   * make a move on the board.
   * @return the new board state, or the old board if the move was not valid.
   */
  def makeMove(i: Int): TicTacToeState[InProgress, InProgress, Unit] = {
    for {
      _ <- moveInternal(i)
      _ <- hasWinner()
    } yield ()
  }

  private def moveInternal(move: Int): TicTacToeState[InProgress, InProgress, Unit] = TicTacToeState {
    status =>
      val newStatus =
        status.board.fields(move) match {
          case Some(_) => status
          case None if move > 8 => status
          case _ => Status[InProgress](status.board.copy(fields = status.board.fields.updated(move, status.playerUp.some)), status.playerUp.succ)
        }
      (newStatus, ()).right
  }

  private def hasWinner(): TicTacToeState[InProgress, InProgress, Unit] = TicTacToeState {
    status =>
      import status._
      val player = playerUp.pred
      val newStatus = {
        computeWinner(board, player) match {
          case true => Status[Finished](board = finish(status.board), playerUp = player).left
          case false => (status, ()).right
        }
      }
      newStatus
  }
}

object Tests extends App {
  import TicTacToe._

  def next = scala.util.Random.nextInt(9)

  val tictactoe = for {
    player <- start()
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
    _ <- makeMove(next)
  } yield ()

  val result = tictactoe.exec(TicTacToe())
  println(result)
}
