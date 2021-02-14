package course6

import course6.HMM.probabilityOfOutcomeForHiddenPath
import org.scalatest.FlatSpec

class HMMSpec extends FlatSpec {
  import HMM._

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

  "return path that maximizes probability" should " return expected result - CASE 1" in {
    val transition = Map[String, Double] (
      "AA" -> 0.641,
      "AB" -> 0.359,
      "BA"-> 0.729,
      "BB"-> 0.271
    )

    val emissionMatrix = Map[String, Double] (
      "AX" -> 0.117,
      "AY" -> 0.691,
      "AZ" -> 0.192,

      "BX" -> 0.097,
      "BY" -> 0.42,
      "BZ" -> 0.483

    )
    val states = List("A", "B")
    val input = "xyxzzxyxyy"
    val expected = "AAABBAAAAA"
    val actual = viterbi(input)(transition, emissionMatrix, states)
    assert(expected === actual)
  }

  "return path that maximizes probability" should " return expected result - CASE 2" in {
    val transition = Map[String, Double] (
      "AA" -> 0.7,
      "AB" -> 0.5,
      "AC"-> 1.0,

      "BA" -> 0.1,
      "BB" -> 0.3,
      "BC"-> 0.0,

      "CA" -> 0.2,
      "CB" -> 0.2,
      "CC"-> 0.0,
    )

    val emissionMatrix = Map[String, Double] (
      "AX" -> 0.0,
      "AY" -> 0.1,

      "BX" -> 0.5,
      "BY" -> 0.5,

      "CX" -> 1.0,
      "CY" -> 0.0

    )
    val states = List("A", "B", "C")
    val input = "xx"
    val expected = "BC"
    val actual = viterbi(input)(transition, emissionMatrix, states)
    assert(expected === actual)
  }

}
