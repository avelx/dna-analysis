package courser4.com.avel.dna

import org.scalatest.FlatSpec

class ProteomicsSpec extends FlatSpec {

  import course4.Proteomics._

  it should "return Graph spectrum" in {
    val intputText = "57 71 154 185 301 332 415 429 486"
    val input = intputText.split(" ").map(_.toInt)
    val result = graphSpectrum(input)
    val expectedResult =
      """|0->57:G
        |0->71:A
        |57->154:P
        |57->185:K
        |71->185:N
        |154->301:F
        |185->332:F
        |301->415:N
        |301->429:K
        |332->429:P
        |415->486:A
        |429->486:G
        |""".stripMargin
    val expectedA = expectedResult.split("\n")
      .map(_.split(":"))
      .map(row => ( row(0).split("->"), row(1) ) )
    val expected = expectedA.map(r => (r._1(0).toInt, r._1(1).toInt ) )

    //expected.foreach( println )
    assert(expected === result)
  }


}
