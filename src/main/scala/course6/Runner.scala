package course6

object Runner extends App {
  val input = List("ATAGA","ATC","GAT")
  val actual = DnaMutation.createTrie(input)
  println( DnaMutation.formattedPrint(actual) )
}