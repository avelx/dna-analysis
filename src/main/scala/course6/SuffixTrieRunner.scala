package course6

object SuffixTrieRunner extends App {
  import SuffixTrie._

  val text = "papa$"
  implicit val result = createTrie( slideText(text) )
  println( formattedPrint  )
}
