object SimplezGenerator {
  import java.io._

  def makeSomeSources(): Seq[(String, String)] = {
    val typeLists = constructTypeLists(('A' to 'X').toList.filterNot(_ == 'F'))
    List(makeGenApApplyFunctions(typeLists))
  }

  def makeGenApApplyFunctions(list : List[List[Char]]) : (String, String) = {
    def apXTemplate(list: List[Char]) : String = {
      val size = list.size - 1

      // [A, B, C]
      val tmpParam = list.mkString("[",",","]")

      // fa: => F[A]
      def param(p : Char) = s"""f${p.toLower}: => F[$p]"""

      // (fa: => F[A], fb: => F[B])
      val paramList = list.init.map(param).mkString("(",",",")")

      // (f: F[(A, B) => C])
      val apFunction = {
        val init = list.init
        val last = list.last
        s"""(f: F[(${init.mkString(",")}) => $last])"""
      }

      val applyFunction = {
        val init = list.init
        val last = list.last
        s"""(f: (${init.mkString(",")}) => $last)"""
      }

      // ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
      //(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
      def apImpl(list : List[Char]) : String = {
        list match {
          case Nil => "(map(f)(_.curried))"
          case head :: tail => s"(ap(f${head.toLower})" + apImpl(tail) + ")"
        }
      }

      // F[C]
      val returnParam = s"""F[${list.last}]"""

      // def ap2[A, B, C](fa: => F[A], fb: => F[B])(f: F[(A, B) => C]): F[C] =
      // (ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
      //def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
      //  ap2(fa, fb)(pure(f))

      s"""
         |  def ap$size$tmpParam$paramList$apFunction : $returnParam =
         |    ${apImpl(list.init.reverse)}
         |
         |  def apply$size$tmpParam$paramList$applyFunction : $returnParam =
         |    ap$size(${list.init.map(c => s"f${c.toLower}").mkString(",")})(pure(f))
       """.stripMargin

    }
    val result =
      s"""
        |package simplez
        |
        |import scala.language.higherKinds
        |
        |trait GenApApplyFunctions[F[_]] {
        |
        |self : Applicative[F] =>
        |
        |${list.map(apXTemplate).mkString("\n")}
        |
        |}
      """.stripMargin
    ("GenApApplyFunctions.scala", result)
  }

  private def constructTypeLists(list : List[Char]) : List[List[Char]] = {
    list match {
      case Nil => Nil
      case head :: x :: Nil =>  Nil
      case head :: _ =>
        val last = list.last
        val init = list.init
        constructTypeLists(init) :+ list
    }
  }
}