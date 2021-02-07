package course6

object BarrowRunner extends  App {

  import  BurrowWheelerTransform._
//
//  //println( suffixArray("panamabananas$") )
  println( suffixArray("banana$") )
//
//  sys.exit()

  //val in2 = "panamabananas$"
  val in2 = "abracadabra$"

  val in = "GTTTCATCCGGGTTTTGCCCCACTGTCGGACCCTCCTTGTAACCTAGATGTTGAACCAACTAAGCTGGATCGAAGGTATGGATTGGATTGAGCTCAGCTGTTCGGGTCCCTACGATGATGCCAGGAATGAATCCGAATTCTTTCTGGCGGCGGTGCAGCGTGTAGTCGGATGCCTTTGGGTATATGGCCAGGTGGACCGATCTGCTGGTAGTTATTCGAAGTGGTAGGGATCGATCAGATCCTCTTGGAACCGGTGCTTTACAGTTCATGGATGTGGCCCCCGCGTCCTGCGGTTAATGGGCCTACGCAAACGGCTCTAACTAAGG$CCCCATCGGTAGCCGAGCGGCCGAAATTCTAAGGTTCTCGGACATGTTGCGGTTAAACATGTCACGCTTCAGGAGTGTCTACACGTGAGCCCGACTATCATGGCTGGTGCGTATGTACCACTCAGAAGCTAAATTGGAGTGGATTGTCACATCTCCCCGGTTTGTGTGTACGACACCTCGTCAGTTTCTGTTTTTGGTTCTCGCAGCCTCCTGTGACTGTGACGAAAGTGGCTTCGCGCCGAATAGCCCCGTTGCTGGAAGGGAGGATACTGGTTGATTATGAAACCTGAGAAAATGCTTTGGAGCTCAGCTCGGTCTTTCCGATCGATACACGCGATCGCGCGGACGTGAATTAAACGCAGAACGTACGAATTGACCTTTTCCGCACTAGTTAGCTCGATGTCGTTTCACTTGGCGATTTTTTCGCTTCATTGAACATGGCTATAGCCTTTAACGCCTGTCTCCTTGCTGCAGGGACTGTCGCCTCATAGAGAGTCTGGCACCGTAAATTGTGTGTATCCAATTTGTATGAAGCTCAGTTTGTTTATTTTGAGTCACTCACGCATGCTGGACATAATCCCTGCTTTATCTCCTCAAGATCACCTGCCCCCCAATGACTTAGGTGTAGTCCTAGTATGTGTAACGTAAAATGACCACGGCGGAAGTTAGATC"
  val in3 =
    """|$Mr Johnson said he was not concerned by Germany's recommendation, adding that the UK's watchdog, the Medicines and Healthcare products Regulatory Agency (MHRA), had "made it very clear" that the AstraZeneca vaccine is "very good and efficacious" and gives a "high degree of protection after just one dose, and even more after two doses".
       |Speaking during a visit to Scotland, the prime minister added: "The evidence that they've supplied is that they think it is effective across all age groups [and] provides a good immune response across all age groups, so I don't agree with that [Germany's recommendation]."
       |An AstraZeneca spokesperson said the latest analyses of clinical trial data for its vaccine "support efficacy in the over 65 years age group", adding that the firm was awaiting "a regulatory decision on the vaccine by the EMA in the coming days".
       |""".stripMargin.toLowerCase()

  //println( in3.length )
  val res = transform("TCAGGGCTTG$")
  println(res)
//  println( compress(res, res.head).length )

  println{
    transformBack("AT$AAACTTCG")
  }

}
