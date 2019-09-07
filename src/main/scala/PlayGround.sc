val alphabet: List[Int] = List(2, 3)
//alphabet.permutations.toList

def combinations(in : List[Int], acc: List[List[Int]]) : List[List[Int]] = {
  val res = for{
    x <- alphabet
    n = x +: in
    if n.sum <= 21
  } yield n
  if (res.isEmpty)
    acc
  else
    acc.map(xs => combinations(xs, acc) ).flatten
}

val res = combinations( List(), List() )

//def combinationAcc(in: List[List[Int]], acc: List[List[Int]]) : List[List[Int]] = in match {
//  case Nil => acc
//  case h::tail =>
//     combinationAcc(tail  , combinations(h) )
//}
//
//val res = combinationAcc( List(alphabet), List() )
//


//val acids = List('X', 'Z')
//
//def peptideVariations(peptide: String): Seq[String] = {
//  for {
//    i <- 1 to peptide.length - 1
//    comb = (peptide + peptide.take(i - 1) ).sliding(i, 1)
//  } yield comb
//  }.flatten
//
//val res = peptideVariations("XZ")
//
//res
//val path = "/Users/pavel/Sources/dna-analysis/src/main/resources/data/graph.txt"
import scala.io.Source

//val filename = path
//val m = Map[Char, Char] ('a' -> '1', 'b' -> '2', 'c' -> '3', 'd' -> '4', 'e' -> '5', 'f' -> '6', 'g' -> '7' )
//for (line <- Source.fromFile(filename).getLines) {
//  //val r = line.replace( m.map(_._2).toArray, m.map(_._1).toArray )
//  //println(r)
//  // replace
//  var l = line
//  m.foreach(p => {
//    l = l.replace(p._1, p._2)
//  })
//  println(l)
//  //println(l.split(":").head.replace(" ", ""))
//}

//val k = m.map(k => (k._2.toString, k._1)).toMap
//List(1, 4, 2, 3, 6, 5, 7)
//  .map(x => k( x.toString ) )