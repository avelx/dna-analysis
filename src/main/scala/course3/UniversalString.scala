import com.dna.assembly.AssemblyFun._

object UniversalString extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def main(args: Array[String]): Unit = {

//    val kmers = Seq(
//      "CTTA",
//      "ACCA",
//      "TACC",
//      "GGCT",
//      "GCTT",
//      "TTAC"
//    )
//
//    val kmers_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_7.txt").getLines()
//      .toList.tail
//
//    val text = stringReconstruction(kmers_)
//    println(text)

    val k = 8

    def format(k: Int, s: String) : String = "0" * (k - s.length) + s

    val kmers = (0 to Math.pow(2.toDouble, k.toDouble).toInt - 1)
      .map(_.toBinaryString)
      .map( format(k, _) )

    //val text = stringReconstruction(kmers.toSeq)
    val kmersIndexes = kmers
      .map(x => Seq(x.init, x.tail) )
      .flatten
      .zipWithIndex
      .toMap[String, Int]

    val kmersIndexesRev = kmersIndexes.toList.map(p => (p._2, p._1) ).toMap[Int, String]

    val g = deBruijnGraphFromKmers(kmers)
    val graph = g.map(y => y.map(x =>kmersIndexes(x) ) )
    val size = graph.flatten.max + 1

    val path = eulerianPath(graph, size)
    val path_ = path.tail.map(i => kmersIndexesRev(i) )
    //println( path_.mkString(" "))
    val res = path_.head.last  + path_.tail.map(_.last).mkString("")
    println(res)
    //val text = pathToGenome( path_ )
    //text
    //println(text)
  }

}
