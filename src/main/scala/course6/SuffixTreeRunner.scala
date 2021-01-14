package course6

object SuffixTreeRunner extends App {
  import SuffixTree._

  //val input = "3 9 0 4 7 1 10 5 2 6".split(" ").sorted
  //println(input mkString(" "))

  // Q5
  val text = "GCCAGCTCTTTCAGTATCATGGAGCCCATGG$"
  println( slideText(text).length  )
//
//  val text = "ATAAATG"
//  implicit val trie = createTrie(slideText(text))
//  println( formattedPrint  )



  //implicit val result = createTrie( slideText("panamabananas$") )
  //println( formattedPrint  )
}
