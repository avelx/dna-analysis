package course6

import course6.HMM.probabilityOfOutcomeForHiddenPath
import org.scalatest.FlatSpec

class HMMSpec extends FlatSpec {
  import HMM.probabilityOfHiddenPath

  "return correct probability of Hidden Path" should " be 0.000384928691755" in {
    val path = "ABABBBAAAA"

    val transition = Map[String, Double] (
      "AA" -> 0.377,
      "AB" -> 0.623,
      "BA"-> 0.26,
      "BB"-> 0.74
    )
    val actual = probabilityOfHiddenPath(path)(transition)
    assert(actual == 0.000384928691755)
  }

  "return probability of an Outcome Given a Hidden Path" should
    " be 3.59748954746e-06" in {
    val emitted = "zzzyxyyzzx"
    val path = "BAAAAAAAAA"

    /*
    	    x	  y	    z
      A	0.176	0.596	0.228
      B	0.225	0.572	0.203
     */

    val emissionMatrix = Map[String, Double] (
      "AX" -> 0.176,
      "AY" -> 0.596,
      "AZ" -> 0.228,

      "BX" -> 0.225,
      "BY" -> 0.572,
      "BZ" -> 0.203

    )
    implicit val accuracy = 50
    val actual = probabilityOfOutcomeForHiddenPath(emitted, path)(emissionMatrix)
    assert(actual == 0.0000035974895474624624)
  }

}
