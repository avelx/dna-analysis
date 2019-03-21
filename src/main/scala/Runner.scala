object Runner {


  def main(args: Array[String]): Unit = {
    import com.binf.Fun._

    val dna = Seq(
      "ATCTAGATCTCGGCTGAAAAAAACTTACCGCCGGCCGTGCTT",
        "AACACTCAACCCTCGGCCTCTTCCCGCGTTGGGCGTACGAGT",
        "TCAGACAAACGGAGTGGTTCCTCGGGGGTCCAACCTTCGGCC",
        "TCAGCCGGGAGCATCTCCACGGGAACGGCCACGGTGAGTAGT",
        "CCTGTGACGGCCTTACTCTCAGGCGTGTGGTATTGTCCGACC",
        "TCGCCATGGGCCCCCCTTTCCGACAGTATCGGACATTCGGCC",
        "CCCATCCTTTTAACTTCAATGGCCGGTTTACCGGCCCGCAGT",
        "ACAGTCGACCCATGGTGATTGAAAAAGAGCTCCGTAACGGCC",
        "CTCTAGACGGCCGGGGGGAAGCTCGCCCGTGTGTTAATCTTC",
        "ATCGGCGTCAGTAGTCAGAAGACTGTCCCCATTGCGACGGCC"
    )

    val result = medianString(dna, 6)
    println(result)

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
