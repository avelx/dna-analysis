package course6

object Runner extends App {
  import  DnaMutation._

//  val data = scala.io.Source
//    .fromFile("/Users/pavel/devcore/sources/dna-analysis/src/test/resources/data/triematch.txt")
//    .getLines().toList
//  val input = List("ATCG", "GGGT")
//  val text = "AATCGGGTTCAATCGGGGT"
// val text = data.head
//  implicit val trie = createTrie(data.tail)

  //println(formattedPrint)
  val text2 = "TTGAATGACTCCTATAACGAACTTCGACATGGCA$"

  val slides = slideText( (text2, 0), List.empty)
  println(slides.map(_._1).mkString("\n"))
  implicit val trie = createTrie(slides.tail.map(_._1))
  //println(formattedPrint)

  println( countLeaves(trie) )


  //println(slides)
  //val slides2 = List(("GGGT",15))

  val res = slides
    .map( prefixTrieMatching(_, trie))
    .filter(_.isDefined)

//  println( res
//    .map(s => s.map(_._2))
//    .filter(_.isDefined)
//    .map(_.get)
//    .mkString(" ")
//  )

  //val actual = prefixTrieMatching(text, trie)
  //println(actual)

  //val rows = DnaMutation.formattedPrint(actual)
  //  .split("\n")

//  val result = rows
//    .map(_.split(":"))
//    .filter(_.length == 2)
//    .map(arr => (arr(0), arr(1)) )
//    .map(p => (p._1.split("->"), p._2) )
//    .map(t => (t._1(0).toInt, t._1(1).toInt, t._2))
//    .sortBy(_._1)
//    .map(t => s"${t._1}->${t._2}:${t._3}")
//    .mkString("\n")
//    .sortBy(_._1)
//    .map(p => s"${p._1}:${p._2}")
//    .mkString("\n")
//  println( result )

}