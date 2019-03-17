object Runner {


  def main(args: Array[String]): Unit = {
    import com.binf.Fun._


        val DNAs = Seq(
          "AGGACAGCTTAAATACCCATCCTTC",
          "CCTCCCCAGGGGGCTCGGTGAGACG",
            "CCTGCCACCAAATATGGCCGAGTCT",
            "CTTCCGAGACAATTGGAGTTCCTGC",
            "TAACTCCTTCCGAATCAGGGAACGA",
            "AACGACCTTCCAGGACAGTCGAAGC"
        )
//    val DNAs = Seq(
//      "ATTTGGC",
//      "TGCCTTA",
//      "CGGTATC",
//      "GAAAATT"
//    )
    val ps = motifEnumeration(DNAs, 5, 1)
    println(ps.mkString(" "))

    //val result = neighbors("GAAGCACGGTC", 2)
    //println(result.mkString(" "))
    //val filePath = "/Users/pavel/Sources/bif/dna-analysis/src/main/resources/data/salmonella.txt"
    //val genome : String = scala.io.Source.fromFile(filePath)
    //  .getLines().toList.tails.mkString("")
    //val res = skewMin(genome)
    //val res : List[String] = freqWordsWithMismatches("ACGTTGCATGTCGCATGATGCATGAGAGCT", 4, 1, true)
    //println( res.distinct.mkString(" ") )
  }

}
