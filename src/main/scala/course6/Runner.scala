package course6

object Runner extends App {
  val input = scala.io.Source
    .fromFile("/Users/pavel/devcore/sources/dna-analysis/src/test/resources/data/patterns.txt")
    .getLines().toList
//  val input = List("ATAGA","ATC","GAT")
  val actual = DnaMutation.createTrie(input)
  val rows = DnaMutation.formattedPrint(actual)
    .split("\n")

  val result = rows
    .map(_.split(":"))
    .filter(_.length == 2)
    .map(arr => (arr(0), arr(1)) )
    .map(p => (p._1.split("->"), p._2) )
    .map(t => (t._1(0).toInt, t._1(1).toInt, t._2))
    .sortBy(_._1)
    .map(t => s"${t._1}->${t._2}:${t._3}")
    .mkString("\n")
//    .sortBy(_._1)
//    .map(p => s"${p._1}:${p._2}")
//    .mkString("\n")

  println( result )
}