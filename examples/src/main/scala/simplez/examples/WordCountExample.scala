package simplez.examples

// from the scalaz examples
// Essence of the iterator pattern, Gibbons & Oliveira
object WordCountExample extends App {

  import simplez._
  import simplez.std.anyVal._
  import simplez.std.list._
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

  // To count, we traverse with a function returning 0 or 1, and sum the results
  // with Monoid[Int], packaged in a constant monoidal applicative functor.
  val Count: Applicative[Lambda[a => Int]] = Monoid[Int].applicative

  // Compose the applicative instance for [a]State[Boolean,a] with the Count applicative
  val WordCount: Applicative[Lambda[a => State[Boolean, Int]]] = State.stateMonad[Boolean].compose[Lambda[a => Int]](Count)

  // Fuse the three applicatives together in parallel...
  implicit val AppCharLinesWord: Applicative[Lambda[a => ((Int, Int), State[Boolean, Int])]] =
    Count // char count
      .product[Lambda[a => Int]](Count) // line count
      .product[Lambda[a => State[Boolean, Int]]](WordCount) // word count

  // ... and execute them in a single traversal
  val ((charCount1, lineCount1), wordCountState1) = Traverse[List].traverse[Lambda[a => ((Int, Int), State[Boolean, Int])], Char, ((Int, Int), State[Boolean, Int])](text)((c: Char) => ((1, test(c == '\n')), atWordStart(c)))
  val ((charCount2, lineCount2), wordCountState2) = text.traverse[Lambda[a => ((Int, Int), State[Boolean, Int])], ((Int, Int), State[Boolean, Int])]((c: Char) => ((1, test(c == '\n')), atWordStart(c)))
  val ((charCount, lineCount), wordCountState) = AppCharLinesWord.traverse(text)((c: Char) => ((1, test(c == '\n')), atWordStart(c)))
  val wordCount: Int = wordCountState.eval(false)

  println("lines: %d\twords: %d\tchars: %d\t".format(lineCount, wordCount, charCount)) // 2        9       35

  val StateWordCount = WordCount.traverse(text)(c => atWordStart(c))
  StateWordCount.eval(false)

}