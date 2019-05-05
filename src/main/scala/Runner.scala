import com.dna.assembly.AssemblyFun._

import scala.io.Source

object Runner extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def main(args: Array[String]): Unit = {

    val kmers = Seq(
      "CTTA",
      "ACCA",
      "TACC",
      "GGCT",
      "GCTT",
      "TTAC"
    )

    val kmers_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_7.txt").getLines()
      .toList.tail

    val text = stringReconstruction(kmers_)
    println(text)

  }

}
