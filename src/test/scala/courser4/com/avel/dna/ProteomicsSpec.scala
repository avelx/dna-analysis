package courser4.com.avel.dna

import org.scalatest.FlatSpec

class ProteomicsSpec extends FlatSpec {

  import course4.Proteomics._

  it should "return Graph spectrum" in {
    val input = "57 71 154 185 301 332 415 429 486"
      .split(" ").map(_.toInt).toList
    val result = graphSpectrum(input)
    val expected = List(
        (154,301),
        (57,154),
        (332,429),
        (429,486),
        (185,332),
        (0,71),
        (71,185),
        (301,429),
        (415,486),
        (301,415),
        (57,185),
        (0,57)
    )
    assert(expected === result)
  }

  it should "return Peptide for ideal spectrum" in {
    val input = "57 71 154 185 301 332 415 429 486"
      .split(" ").map(_.toInt).toList
    val expected = "GPFNA"
    val result = decodingIdealSpectrum(input)
    assert(expected === result)
  }


}
