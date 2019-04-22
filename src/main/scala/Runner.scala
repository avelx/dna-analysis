import scala.io.Source

object Runner {

  def main(args: Array[String]): Unit = {

//    val kmers = Seq(
//      "GAGG",
//      "CAGG",
//      "GGGG",
//      "GGGA",
//      "CAGG",
//      "AGGG",
//      "GGAG"
//    )
    val kmers = Source.fromFile("/Users/pavel/Sources/bif/dna-analysis/src/main/resources/data/dataset_200_8.txt").getLines.toList
    import com.dna.assembly.AssemblyFun._
    val result = deBruijnGraphFromKmers(kmers)
    println(result)

    //val kmers = Source.fromFile("/Users/pavel/Sources/bif/dna-analysis/src/main/resources/data/dataset_200_8.txt").getLines


    //    val source = scala.io.Source.fromFile("/Users/pavel/Sources/bif/dna-analysis/src/main/resources/upstream250.txt")
    //    val dna = source.getLines().filter(l => !l.contains(">Rv")).map(_.trim).toList
    //
    //    val res = randomizedMotifSearchFull(dna, 20, 8)(500)
    //
    //    println( res.mkString("\n") )

    //
    //    val matrix = Array(
    //      Array(0.4, 0.3, 0.0, 0.1, 0.0, 0.9),
    //      Array(0.2, 0.3, 0.0, 0.4, 0.0, 0.1),
    //      Array(0.1, 0.3, 1.0, 0.1, 0.5, 0.0),
    //      Array(0.3, 0.1, 0.0, 0.4, 0.5, 0.0)
    //    )
    //    println( pr(matrix, "TCGGTA") )
    //    val dna = Seq(
    //      "CTCGATGAGTAGGAAAGTAGTTTCACTGGGCGAACCACCCCGGCGCTAATCCTAGTGCCC",
    //        "GCAATCCTACCCGAGGCCACATATCAGTAGGAACTAGAACCACCACGGGTGGCTAGTTTC",
    //        "GGTGTTGAACCACGGGGTTAGTTTCATCTATTGTAGGAATCGGCTTCAAATCCTACACAG"
    //    )
    //    println( medianString(dna, 7) )
    //    val p : Array[Array[Double]] = Array(
    //      Array(0.5, 0, 0, 0.5),
    //      Array(0.25, 0.25, 0.25, 0.25),
    //      Array(0, 0, 0, 1),
    //      Array(0.25, 0, 0.5, 0.25))
    //    val res = entroty(p)
    //    println( res.mkString(" "))
  }

}
