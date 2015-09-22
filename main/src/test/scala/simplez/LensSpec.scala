package simplez

import std.option._
import org.specs2.mutable._
import org.specs2.time.NoTimeConversions

class LensSpec extends Specification with NoTimeConversions {

  case class Person(name: String, age: Int, other: Other)
  case object Person {
    val _name = Lens[Person, String](_.name, (p, n) => p.copy(name = n))
    val _age = Lens[Person, Int](_.age, (p, a) => p.copy(age = a))
    val _other = Lens[Person, Other](_.other, (p, o) => p.copy(other = o))
  }

  case class Other(value: Double)
  case object Other {
    val _value = Lens[Other, Double](_.value, (o, v) => o.copy(value = v))
  }

  val person = Person("Test", 42, Other(39.0))

  import Person._
  import Other._

  "A Lens" should {
    "be able to access a field" in {
      _name.get(person) must beEqualTo("Test")
    }

    "be able to set a field" in {
      _name.set(person, "Test2") must beEqualTo(person.copy(name = "Test2"))
    }

    "be able to modify a field" in {
      _name.modify(n => n + n)(person) must beEqualTo(person.copy(name = "TestTest"))
    }

    "be able to modifyF a field" in {
      _name.modifyF(n => Option(n + n))(person) must beEqualTo(Some(person.copy(name = "TestTest")))
    }

    "be able to compose" in {
      val _lens = _other.composeLens(_value)
      _lens.modify { _ + 1 }(person) must beEqualTo(person.copy(other = Other(40.0)))
    }
  }

  "Traditional way" should {
    import scala.concurrent._
    import scala.concurrent.duration._
    import std.future._
    case class Person(animal: Animal)
    case class Animal(dead: Boolean)
    def schrödinger(d: Boolean): Future[Boolean] = Future { !d }
    val someP = Person(Animal(false))

    "be less awesome" in {

      val newP: Future[Person] = {
        for {
          fp <- Future.successful(someP)
          fa <- Future.successful(fp.animal)
          fd <- schrödinger(fa.dead)
        } yield (fp.copy(animal = fa.copy(dead = fd)))
      }

      Await.result(newP, 500 millis) must beEqualTo(Person(Animal(true)))
    }

    "than lens way" in {
      val _animal = Lens[Person, Animal](_.animal, (p, a) => p.copy(animal = a))
      val _dead = Lens[Animal, Boolean](_.dead, (a, d) => a.copy(dead = d))

      val newP = _animal.composeLens(_dead).modifyF(schrödinger _)(someP)
      Await.result(newP, 500 millis) must beEqualTo(Person(Animal(true)))
    }
  }
}