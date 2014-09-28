package simplez.examples

import simplez._
import simplez.syntax._
import simplez.std.string._
import simplez.id._

object IOExample extends App {

  // define the "program" which you want to execute without side effects
  sealed trait Console[A]
  case object ReadLine extends Console[String]
  case class PutLine(s: String) extends Console[Unit]

  
  type ConsoleIO[A] = Free[Console, A]
  def readLine(): ConsoleIO[String] = Bind(ReadLine, (s: String) => Return[Console, String](s))
  def putLine(s: String): ConsoleIO[Unit] = Bind(PutLine(s), (_: Unit) => Return[Console, Unit](()))

  object RealConsole extends (Console ~> Id) {
    override def apply[A](F: Console[A]): Id[A] = {
      F match {
        case ReadLine => scala.io.StdIn.readLine()
        case PutLine(s) => println(s)
      }
    }
  }

  val prg: ConsoleIO[Unit] = for {
    _      <- IOExample.putLine("Enter your name:")
    name   <- IOExample.readLine()
    _      <- IOExample.putLine(s"Hello, " + name)
  } yield ()

  val realResult: Unit = prg.foldMap(RealConsole)

  // a test class consisting of predefined inputs and outputs
  case class Inout(in: List[String], out: List[String])
  type InoutState[A] = State[Inout, A]

  // a state manipulating TestConsole
  object TestConsole extends (Console ~> InoutState) {
    override def apply[A](F: Console[A]): InoutState[A] = {
      State(c => {
        (c, F) match {
          case (Inout(in, out), ReadLine) => (Inout(in.tail, out), in.head)
          case (Inout(in, out), PutLine(s)) => (Inout(in, out :+ s), ())
        }
      })
    }
  }
  
  val initialState = Inout(List("Markus"), List.empty[String])
  val prgState: InoutState[Unit] = prg.runM(TestConsole)

  val result: Inout = prgState.exec(Inout(List("Markus"), List.empty[String]))
  println(result)
  assert(result.out == List("Enter your name:", "Hello, Markus"))

}
