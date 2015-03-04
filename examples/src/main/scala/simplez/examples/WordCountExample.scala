package simplez.examples

// from the scalaz examples
// Essence of the iterator pattern, Gibbons & Oliveira
object WordCountExample extends App {

  import simplez._
  import simplez.std.anyVal._
  import simplez.std.list._

  def test(b: Boolean): Int = if (b) 1 else 0

  val text = "the cat in the hat\n sat on the mat\n".toList

  // To count words, we need to detect transitions from whitespace to non-whitespace.
  def atWordStart(c: Char): State[Boolean, Int] = State { (prev: Boolean) =>
    val cur = c != ' '
    (cur, test(cur && !prev))
  }

  // To count, we traverse with a function returning 0 or 1, and sum the results
  // with Monoid[Int], packaged in a constant monoidal applicative functor.
  val Count = Monoid[Int].applicative

  // Compose the applicative instance for [a]State[Boolean,a] with the Count applicative
  val WordCount = State.stateMonad[Boolean].compose[Lambda[a => Int]](Count)

  // Fuse the three applicatives together in parallel...
  val AppCharLinesWord = Count
    .product[Lambda[a => Int]](Count)
    .product[Lambda[a => State[Boolean, Int]]](WordCount)

  // ... and execute them in a single traversal
  val ((charCount, lineCount), wordCountState) = AppCharLinesWord.traverse(text)((c: Char) => ((1, test(c == '\n')), atWordStart(c)))
  val wordCount = wordCountState.eval(false)

  println("%d\t%d\t%d\t".format(lineCount, wordCount, charCount)) // 2        9       35

}