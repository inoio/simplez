object SimplezGenerator {

  implicit class HelperFunctionsList(list: List[Char]) {
    // (f: F[(A, B) => C])
    def apFunction(): String = {
      val init = list.init
      val last = list.last
      s"""(f: F[(${init.mkString(",")}) => $last])"""
    }

    def applyFunction() = {
      val init = list.init
      val last = list.last
      s"""(f: (${init.mkString(",")}) => $last)"""
    }

    // (fa: => F[A], fb: => F[B])
    val typedParamList = list.init.map(c => c.param).mkString("(", ",", ")")

    // (A, B, C)
    def toParamList(): String = list map (c => c.toUpper) mkString("(", ",", ")")

    val paramList = list.init.map(c => s"f${c.toLower}")

    // [A, B, C]
    def typeList = list.mkString("[", ",", "]")
  }

  implicit class HelperFunctionsChar(c: Char) {
    def valName() = c.toLower

    // fa: => F[A]
    def param() = s"""f${c.toLower}: => F[$c]"""

    def typeName() = s"F[$c]"
  }

  def makeSomeSources(): Seq[(String, String)] = {
    val typeLists = constructTypeLists(('A' to 'X').toList.filterNot(_ == 'F'))
    
    List(makeGenApApplyFunctions(typeLists)) //, makeApplicativeTraits(typeLists last))
  }

  def makeApplicativeTraits(current: List[Char]): (String, String) = {

      def result(types: List[Char]): String = {
        val paramSize = types.size - 1
        val paramTypes = types init
        val lastParamType = paramTypes.last.toUpper
        val nextParamType = paramTypes.drop(1).last.toUpper
        val valName = paramTypes.last.toLower
        val nextValName = types.last.toLower
        val nextValNameDouble = nextValName.toString + nextValName.toString
        val valNameType = paramTypes.last.toUpper
        val returnType = types.last.toUpper
        val callParams = paramTypes.map(_.toLower).mkString("(",",", ")")

        /*
         * A,B,C,D
         * sealed trait ApplicativeBuilder3[C] {
      val c: F[C]

      def apply[D](f: (A, B, C) => D)(implicit ap: Applicative[F]): F[D] = ap.apply3(a, b, c)(f)

      def tupled(implicit ap: Applicative[F]): F[(A, B, C)] = apply(Tuple3.apply)

    }
         */
        val traitString =
          if (types.size > 4) {
            result(types.init) +
              s"""
      |
      |sealed trait ApplicativeBuilder$paramSize[$lastParamType] {
      |      val $valName: F[$valNameType]
      |
      |      def apply[$returnType](f: ${paramTypes.toParamList} => $returnType)(implicit ap: Applicative[F]): F[$returnType] = ap.apply$paramSize$callParams(f)
      |
      |      def tupled(implicit ap: Applicative[F]): F[${paramTypes.toParamList}] = apply(Tuple${paramSize}.apply)
      |
      |      def |@|[$returnType](${nextValNameDouble}: F[${returnType}]) = new ApplicativeBuilder${paramSize +1}[${returnType}] {
      |        val ${nextValName} = ${nextValNameDouble}
      |      }

      |
    """.stripMargin
          }
            else {
              s"""
      |sealed trait ApplicativeBuilder$paramSize[$lastParamType] {
      |      val $valName: F[$valNameType]
      |
      |      def apply[$returnType](f: ${paramTypes.toParamList} => $returnType)(implicit ap: Applicative[F]): F[$returnType] = ap.apply$paramSize$callParams(f)
      |
      |      def tupled(implicit ap: Applicative[F]): F[${paramTypes.toParamList}] = apply(Tuple${paramSize}.apply)
      |""".stripMargin
          }
        traitString
      }
    val r = "package simplez\n" + result(current)
    println(r)
    ("GenApplicatives.scala", r)
  }

  def makeGenApApplyFunctions(list: List[List[Char]]): (String, String) = {
    def apXTemplate(list: List[Char]): String = {
      val size = list.size - 1

      // ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
      //(ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
      def apImpl(list: List[Char]): String = {
        list match {
          case Nil => "(map(f)(_.curried))"
          case head :: tail => s"(ap(f${head.toLower})" + apImpl(tail) + ")"
        }
      }


      // def ap2[A, B, C](fa: => F[A], fb: => F[B])(f: F[(A, B) => C]): F[C] =
      // (ap(fc)(ap(fb)(ap(fa)(map(f)(_.curried))))
      //def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C): F[C] =
      //  ap2(fa, fb)(pure(f))

      s"""|  def ap$size${list.typeList}${list.typedParamList}${list.apFunction} : ${list.last.typeName} =
         |    ${apImpl(list.init.reverse)}
         |
         |  def apply$size${list.typeList}${list.typedParamList}${list.applyFunction} : ${list.last.typeName} =
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

  private def constructTypeLists(list: List[Char]): List[List[Char]] = {
    list match {
      case Nil => Nil
      case head :: x :: Nil => Nil
      case head :: _ =>
        val last = list.last
        val init = list.init
        constructTypeLists(init) :+ list
    }
  }
}