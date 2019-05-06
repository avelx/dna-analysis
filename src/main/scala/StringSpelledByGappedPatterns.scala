
import com.dna.assembly.AssemblyFun._

import scala.io.Source

object StringSpelledByGappedPatterns extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def main(args: Array[String]): Unit = {

    val kmersA = Seq("GACC", "ACCG", "CCGA", "CGAG", "GAGC")
    val kmersB = Seq("GCGC", "CGCC", "GCCG", "CCGG", "CGGA")

    //    val kmers_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_7.txt").getLines()
    //      .toList.tail

    val textA = stringReconstruction(kmersA)
    val textB = stringReconstruction(kmersB)

    println(textA)
    println(textB)

  }

}