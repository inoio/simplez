package simplez.examples.composition

import scalaz._
import scalaz.syntax.functor._

import scalaz.std.scalaFuture._
import scalaz.std.option._
import scalaz.syntax.std.option._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.UUID

case class Person(personId: UUID, name: String, firstName: String)

case class Order(orderId: UUID, personId: UUID, data: AnyRef)

case class SomethingElse(i: Int)

object DBFunctions {

  val x = List(1, 2, 3, 4, 5)

  def findPerson(id: UUID): Future[Option[Person]] = ???

  def findOrderById(orderId: UUID): Future[Option[Order]] = ???

  def findOrderByPerson(id: UUID): Future[List[Order]] = ???

  def findSomethingElse(i: Int): Future[Option[SomethingElse]] = ???

  def getPersonName(id: UUID): Future[Option[String]] = {
    import scalaz.std.option._
    import scalaz.std.list._
    val FO = Functor[Future].compose[Option]
    FO.map(findPerson(id))(_.name)
  }

  def getPersonNameAndFirstName(id: UUID): Future[Option[(String, String)]] = {
    import scalaz.std.option._
    import scalaz.std.list._
    val FO = Functor[Future].compose[Option]
    FO.map(findPerson(id))(p => (p.name, p.firstName))
  }

  def getPersonAndASingleOrder(personId: UUID, orderId: UUID): Future[Option[(Person, Order)]] = {
    import scalaz.syntax.apply._
    import scalaz.std.option._
    import scalaz.syntax.std.option._

    implicit val applicative = Applicative[Future].compose[Option]

    applicative.apply2(findPerson(personId), findOrderById(orderId)) { case (p, o) => (p, o) }

    (findPerson(personId) |@| findOrderById(orderId)) { ??? }

  }
  import scalaz.std.list._
  import scalaz.std.option._

  val FOL = Functor[Option].product[List]
  FOL.map((3.some, List(1, 2, 3)))(_ + 4)

  val AOL: Applicative[Lambda[a => (Option[a], List[a])]] = Applicative[Option].product[List]
  AOL.ap(3.some -> List(1, 2, 3))(((x: Int) => x + 5).some -> List((x: Int) => x + 6))
  AOL.traverse(List(1, 2, 3))(x => (x + x).some -> List(-x, x))

}