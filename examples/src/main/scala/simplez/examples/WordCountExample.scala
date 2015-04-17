package simplez.examples

// from the scalaz examples
// Essence of the iterator pattern, Gibbons & Oliveira
object WordCountExample extends App {
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import simplez._
  import simplez.std.anyVal._
  import simplez.std.list._
  import simplez.std.future._
  import simplez.std.option._
  import simplez.syntax._

  def test(b: Boolean): Int = if (b) 1 else 0

  //char count            11111111111111.....
  //line count            0000000000000000001 0000000000000001
  //word count            1000100010010001....
  val text: List[Char] = "the cat in the hat\n sat on the mat\n".toList

  // To count words, we need to detect transitions from whitespace to non-whitespace.
  def atWordStart(c: Char): State[Boolean, Int] = State { (prev: Boolean) =>
    val cur = c != ' '
    (cur, test(cur && !prev))
  }

  val stateApplicative: Applicative[Lambda[a => State[Boolean, a]]] = State.stateMonad[Boolean]

  // char count
  val Count: Applicative[Lambda[a => Int]] = Monoid[Int].applicative
  val charResult: Int = Count.traverse(text)(_ => 1)
  println(s"$charResult  == ${text.length}")

  // line count
  val lineResult: Int = Count.traverse(text)(c => if (c == '\n') 1 else 0)
  println(s"$lineResult")

  // product of char count, line count
  val charLineCount: Applicative[Lambda[a => (Int, Int)]] = Count.product[Lambda[a => Int]](Count)
  val (cc, lc) = charLineCount.traverse(text)(c => (1, if (c == '\n') 1 else 0))
  println(s"$cc, $lc")

  // word count
  // step 1 => State[Boolean, List[Int]]
  val wordCountA: Applicative[Lambda[a => State[Boolean, Int]]] = stateApplicative.compose[Lambda[a => Int]](Count)
  val (_, listOfInts: List[Int]) = stateApplicative.traverse(text)(atWordStart).run(false)

  // step 2 : compose directly   
  val wordCount: Applicative[Lambda[a => State[Boolean, Int]]] = stateApplicative.compose[Lambda[a => Int]](Count).compose[Lambda[a => Int]](Count)
  val word = wordCount.traverse(text)(atWordStart).run(false)
  println(wordCount)

  // product of char count, line count and word count
  val Final = Count.product[Lambda[a => Int]](Count).product[Lambda[a => State[Boolean, Int]]](wordCount)
  val ((ccF, lcF), wcF) = Final.traverse(text)(c => ((1 -> test(c == '\n')) -> atWordStart(c)))
  val wcFF = wcF.run(false)
}