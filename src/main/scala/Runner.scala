import com.dna.assembly.AssemblyFun._

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
    val text = stringReconstruction(kmers)
    println(text)
  }

}
