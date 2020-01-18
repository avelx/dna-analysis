package course4

object ParsimonyApp extends App {
  import course4.EvolutionaryTree.parsymonyScore

  type Tree = Map[Int, Int]
  type Leaf = String

  val alphabet = Seq('A', 'C', 'G', 'T')

  case class Leafs(left: String, right: String)

  val tree = Map[Int, Int](6 -> 4, 6 -> 5)
  val leafs = Map[Int, Leafs](
    4 -> Leafs("CAAATCCC", "ATTGCGAC"),
    5 -> Leafs("CTGCGCTG", "ATGGACGA")
  )

  val dna : List[String] = leafs
    .values.map(x => List(x.left, x.right) ).flatten.toList

  val (score, minStr) = parsymonyScore(dna)(alphabet)
  println(minStr)

}