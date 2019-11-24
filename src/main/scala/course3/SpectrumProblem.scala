import scala.io.Source

object SpectrumProblem {

  val integetMassPath: String = "/Users/pavel/Sources/dna-analysis/src/main/resources/data/integer_mass_table.txt"

  def massTable(): Map[String, String] =
    Source.fromFile(integetMassPath)
      .getLines()
      .toList
      .map(_.split(" "))
      .map(x => (if (x.length == 2) (x(0), x(1)) else (x(0), "")))
      .toMap[String, String].withDefaultValue("")

  def peptideVariations(peptide: String): Seq[String] = {
    for {
      i <- 1 to peptide.length - 1
      comb = (peptide + peptide.take(i - 1) ).sliding(i, 1)
    } yield comb
  }.flatten



  def main(args: Array[String]) = {

//    val table = massTable()
//    val rna = "CAACAG"
//    val res = rna.map(c => table( c.toString() ).toInt ).sum
//
//    println(res)


    val peptide = "TAIM"
    val variations = peptideVariations(peptide)
    val table = massTable()
    val res = (variations :+ peptide).map(item =>  item.map(c => table(c.toString).toInt).sum )
      .toList
      .sorted

    println(res.mkString(" "))

    println( res.length )
    val spec = "0 71 101 113 131 184 202 214 232 285 303 315 345 416".split(" ").map(_.toInt)

    val res2 = (0 +: res)

    val r = spec.zip(res2).forall(p => p._1 == p._2)

    println(r)


    //assert(spec == res2)
    //println( res.length )
    //println( "0 " + res.mkString(" ")  )

    //println( variations.mkString(" ") )

  }

}
