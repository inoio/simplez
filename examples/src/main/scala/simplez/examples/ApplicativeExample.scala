package simplez.examples

import simplez._
import std.option._
import syntax._

case class CaseClassAbc(a: Int, b: Int, c: Int)

object ApplicativeExample extends App {

  val aOpt = Some(3)
  val bOpt = Some(5)
  val cOpt = Some(7)

  Applicative[Option].apply3(aOpt, bOpt, cOpt)((a, b, c) => CaseClassAbc(a, b, c))
  Applicative[Option].apply3(aOpt, bOpt, cOpt)(CaseClassAbc.apply)
  Applicative[Option].apply3(aOpt, bOpt, cOpt)(CaseClassAbc)

}
