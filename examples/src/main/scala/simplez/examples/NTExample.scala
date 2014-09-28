package simplez.examples

import simplez._
import syntax._
import id._

object NTExample extends App {

  sealed trait Expr[A]
  case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]
  case class Or(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean]
  case class Not(a: Expr[Boolean]) extends Expr[Boolean]
  case object True extends Expr[Boolean]
  case object False extends Expr[Boolean]

  sealed trait PrintExpr[A]
  case class Print(a: Expr[Boolean]) extends PrintExpr[String]

  val x = (And(True, Not(False)))

  object EvaluateExpr extends (Expr ~> Id) {
    def apply[A](e: Expr[A]): Id[A] = {
      e match {
        case True => true
        case False => false
        case Not(a) => !EvaluateExpr(a)
        case And(a, b) => EvaluateExpr(a) && EvaluateExpr(b)
        case Or(a, b) => EvaluateExpr(a) || EvaluateExpr(b)
      }
    }
  }

  println(EvaluateExpr(x))

}