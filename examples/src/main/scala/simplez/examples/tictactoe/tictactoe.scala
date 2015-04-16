package simplez.examples.tictactoe

import scalaz._
import Scalaz._

sealed trait Player {
  /** the strategy defines how a player moves, given a board constellation. */
  val strategy: Board[InProgress] => Option[Int]
}

case object Player1 extends Player {
  override val strategy = (board: Board[InProgress]) =>
    Player.occupyMiddleStrategy(board) orElse
      Player.occupyCornerStrategy(board) orElse
      Player.randomStrategy(board)

  override def toString(): String = "X"
}

case object Player2 extends Player {
  //  val strategy = Player.manualStrategy _
  override val strategy = (board: Board[InProgress]) =>
    Player.occupyMiddleStrategy(board) orElse
      Player.occupyCornerStrategy(board) orElse
      Player.randomStrategy(board)

  override def toString(): String = "O"
}

case object Player {
  def manualStrategy(board: Board[InProgress]): Option[Int] = {
    println(board)
    println("Enter your field (0-8): ")
    scala.io.StdIn.readInt().some
  }

  def randomStrategy(board: Board[InProgress]): Option[Int] = {
    scala.util.Random.nextInt(9).some
  }

  def occupyMiddleStrategy(board: Board[InProgress]): Option[Int] = {
    import Board._
    if (!isOccupied(board, 4)) 4.some else none[Int]
  }

  def occupyCornerStrategy(board: Board[InProgress]): Option[Int] = {
    import Board._
    def toOption(b: Boolean) = if (b) b.some else none[Boolean]
    val corners = List(0, 2, 6, 8)
    corners.flatMap { index => toOption(!isOccupied(board, index)).map(_ => index) }.headOption
  }

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
final case class Status[S <: BoardState](board: Board[S], playerUp: Player, msg: Option[String] = none[String])

final case class Board[State <: BoardState] private (fields: List[Field]) {
  override def toString() = {
    val line = "\n-------\n"
    val boardAsStrings = for {
      field <- fields
    } yield {
      field match {
        case Some(\/-(p)) => p.toString
        case Some(-\/(p)) => p.toString
        case _ => " "
      }
    }
    boardAsStrings.sliding(3, 3).toList.map(row => row.mkString("|", "|", "|")).mkString(line, line, line)
  }
}

final case object Board {
  /** create an empty board. */
  def apply(): Board[Empty] = Board[Empty](List.fill(9)(None))
  def asRows(board: Board[_]): List[List[Field]] = board.fields.sliding(3, 3).toList
  def asColumns(board: Board[_]): List[List[Field]] = asRows(board).transpose
  implicit def convertTupleToPos(t: (Int, Int)): Int = t._1 + t._2 * 3

  def isOccupied(board: Board[InProgress], index: Int): Boolean = {
    Traverse[Option].count(board.fields(index)) == 1
  }
}

final case object TicTacToe {
  import Player._
  import Board._

  object TicTacToeState {
    def apply[S1 <: BoardState, S2 <: BoardState, A](f: Status[S1] => EitherBoard[(Status[S2], A)]) = IndexedStateT[EitherBoard, Status[S1], Status[S2], A] { f }
  }

  private def inProgress(board: Board[Empty]): Board[InProgress] = board.copy()
  private def finish(board: Board[InProgress]): Board[Finished] = board.copy()

  val stateMonad = IndexedStateT.stateTMonadState[Status[InProgress], EitherBoard]
  import stateMonad._
  /**
   * create an empty board.
   */
  def apply(): Status[Empty] = Status[Empty](Board(), Player1)

  /**
   * @return true if player is a winner of this game
   */
  private def computeWinner(board: Board[InProgress], player: Player): Boolean = {
    import scalaz.syntax.foldable._
    def diag1(fields: List[Field]): List[Field] = fields(0 -> 0) :: fields(1 -> 1) :: fields(2 -> 2) :: Nil
    def diag2(fields: List[Field]): List[Field] = fields(2 -> 0) :: fields(1 -> 1) :: fields(0 -> 2) :: Nil
    val allRows: Rows = asRows(board)
    val allColumns: Rows = asColumns(board)
    val allDiags: Rows = List(diag1(board.fields), diag2(board.fields))
    val all: Rows = allRows ++ allColumns ++ allDiags
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

  private def computeDraw(board: Board[InProgress]): Boolean = {
    val F = Traverse[List].compose[Option]
    F.count(board.fields) == 9
  }

  def moveUntilFinished(): TicTacToeState[InProgress, InProgress, Unit] = {
    whileM_[Unit](point(true), makeMove)
  }

  /**
   * start a game of Tic Tac Toe
   */
  def start(): TicTacToeState[Empty, InProgress, Unit] = TicTacToeState {
    status =>
      (Status[InProgress](board = inProgress(status.board), playerUp = status.playerUp), ()).right
  }

  def setup(firstPlayer: Player = Player1): TicTacToeState[Empty, Empty, Unit] = TicTacToeState {
    status =>
      println(s"${firstPlayer} starts.")
      (status.copy(playerUp = firstPlayer): Status[Empty], ()).right
  }

  /**
   * make a move on the board.
   * @return the new board state, or the old board if the move was not valid.
   */
  def makeMove(): TicTacToeState[InProgress, InProgress, Unit] = {
    for {
      playerUp <- gets(status => status.playerUp)
      _ <- untilM_[Boolean](moveInternal(playerUp), moveInternal(playerUp))
      _ <- hasWinner()
      _ <- hasDraw()
      _ <- switchBoard()
    } yield {
      ()
    }
  }

  private def switchBoard(): TicTacToeState[InProgress, InProgress, Unit] = TicTacToeState {
    status =>

      val F = Functor[List].compose[Option]
      val updatedFields = F.map(status.board.fields)(f => f.swap)
      val newStatus = Status[InProgress](status.board.copy(fields = updatedFields), status.playerUp.pred)
      (newStatus, ()).right

  }

  private def moveInternal(player: Player): TicTacToeState[InProgress, InProgress, Boolean] = TicTacToeState {
    status =>
      val move = player.strategy(status.board).get
      val (newStatus, validMove) =
        status.board.fields(move) match {
          case Some(_) =>
            println("Illegal Move, field occupied"); (status, false)
          case _ if move > 8 =>
            println("Illegal move, > 8"); (status, false)
          case _ => (Status[InProgress](status.board.copy(fields = status.board.fields.updated(move, status.playerUp.right.some)), status.playerUp.succ), true)
        }
      (newStatus, validMove).right
  }

  private def hasWinner(): TicTacToeState[InProgress, InProgress, Unit] = TicTacToeState {
    status =>
      import status._
      val player = playerUp.pred
      if (computeWinner(board, player)) {
        Status[Finished](board = finish(status.board), playerUp = player, s"$player wins.".some).left
      } else {
        (status, ()).right
      }
  }

  private def hasDraw(): TicTacToeState[InProgress, InProgress, Unit] = TicTacToeState {
    status =>
      import status._
      val player = playerUp.pred

      if (computeDraw(board)) {
        Status[Finished](board = finish(status.board), playerUp = player, s"Draw.".some).left
      } else {
        (status, ()).right
      }

  }
}

object Tests extends App {
  import TicTacToe._

  val tictactoe = for {
    _ <- setup(Player2)
    _ <- start()
    _ <- moveUntilFinished()
  } yield ()

  val result = tictactoe.exec(TicTacToe())
  println(result)
}
