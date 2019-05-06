import com.dna.assembly.AssemblyFun._

import scala.io.Source

object Runner extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def main(args: Array[String]): Unit = {

    val kmers = Seq(
      "AAAT",
      "AATG",
      "ACCC",
      "ACGC",
      "ATAC",
      "ATCA",
      "ATGC",
      "CAAA",
      "CACC",
      "CATA",
      "CATC",
      "CCAG",
      "CCCA",
      "CGCT",
      "CTCA",
      "GCAT",
      "GCTC",
      "TACG",
      "TCAC",
      "TCAT",
      "TGCA"
    )

    //    val kmers_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_7.txt").getLines()
    //      .toList.tail

    //    val text = stringReconstruction(kmers)
    //    println(text)
    val kmersIndexes = kmers
      .map(x => Seq(x.init, x.tail))
      .flatten
      .zipWithIndex
      .toMap[String, Int]

    val kmersIndexesRev = kmersIndexes.toList.map(p => (p._2, p._1)).toMap[Int, String]

    val g = deBruijnGraphFromKmers(kmers)
    val graph = g.map(y => y.map(x => kmersIndexes(x)))
    val size = graph.flatten.max + 1

    val path = eulerianPath(graph, size)
    val path_ = path.map(i => kmersIndexesRev(i))

    val res = path_.last +: path_.init
    //val text = pathToGenome(path_)
    println("")

    val assembeld = res.head + res.tail.map(_.last).mkString("")
    println(assembeld)

  }

}
