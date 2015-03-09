package simplez.examples

object LoopExample extends App {

  /**
   * public static int loophMyObji (IEnumerablehMyObji coll){
   * int n = 0;
   * foreach (MyObj obj in coll){
   * n = n+ 1;
   * obj.touch ();
   * }
   * return n;
   * }
   */

  import simplez._
  import std.list._
  import syntax._

  case class Person(id: Int, version: Int = 0)
  case object Person {
    def touch(p: Person) = p.copy(version = p.version + 1)
  }

  val list = (1 to 10).map(id => Person(id)).toList

  def loop(fa: List[Person]): (Int, List[Person]) = {
    implicit val counterState: State.StateMonad[Int] = State.stateMonad[Int]
    import counterState._
    val counter = (p: Person) => (for {
      _ <- modify(c => c + 1)
    } yield ())

    fa.collectS[Int, Person](counter)(Person.touch _).run(0)
  }

  def label(fa: List[Person]) = {
    implicit val labelState = State.stateMonad[Int]
    import labelState._
    val labelCounter: State[Int, Int] = for {
      n <- get
      _ <- put(n + 1)
    } yield n

    val labelling: Person => Int => String = person => counter => s"$counter. $person"
    fa.disperseS[Int, String](labelCounter, labelling).eval(1)
  }

  println(s"loop: ${loop(list)}")
  println(s"label: ${label(list)}")

}