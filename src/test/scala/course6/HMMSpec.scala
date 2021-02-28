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
    val alphabet = List("X", "Y", "Z")
    val actual = viterbi(input)(transition, emissionMatrix, states, alphabet)
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
    val alphabet = List("X", "Y", "Z")
    val actual = viterbi(input)(transition, emissionMatrix, states, alphabet)
    assert(expected === actual)
  }

  "return path that maximizes probability" should " return expected result - CASE 3" in {

    val transition = Map[String, Double] (
      "AA" -> 0.435,
      "AB" -> 0.483,
      "AC" -> 0.082,
      //    "AD" -> 0.21,

      "BA"-> 0.501,
      "BB"-> 0.035,
      "BC"-> 0.464,
      //    "BD" -> 0.21,

      "CA"-> 0.393,
      "CB"-> 0.409,
      "CC"-> 0.198
      //    "CD" -> 0.278,

      //  "DA"-> 0.322,
      //  "DB"-> 0.281,
      //  "DC"-> 0.138,
      //  "DD" -> 0.302
    )

    val emissionMatrix = Map[String, Double] (
      "AX" -> 0.417,
      "AY" -> 0.203,
      "AZ" -> 0.38,

      "BX" -> 0.367,
      "BY" -> 0.266,
      "BZ" -> 0.367,

      "CX" -> 0.446,
      "CY" -> 0.446,
      "CZ" -> 0.108

      //  "DX" -> 0.276,
      //  "DY" -> 0.268,
      //  "DZ" -> 0.456

    )
    val input = "yyyzzzyxyzzyyyyxzzzyzxyyzyxzzzyxyxxyzyzyyzzxxzzxyzyxzxzxzxyxzzyzyyyzxyyxxzyxxyxxxyyyxzyxzzyzzyzxxxzx"
    val expected = "CBCBABCBCBABCBCABABCBABCBCABABCBCABCBCBCBABAABABCBCABABAABCBABCBCBCBABCBABCABCBABCBCABCBABCABCBABABA"
    val states = List("A", "B", "C")
    val alphabet = List("X", "Y", "Z")
    val actual = viterbi(input)(transition, emissionMatrix, states, alphabet)
    assert(expected == actual)
  }

  "return path that maximizes probability" should " return expected result - CASE 4" in {
    val transition = Map[String, Double] (
      "AA" -> 0.315,
      "AB" -> 0.086,
      "AC" -> 0.228,
      "AD" -> 0.371,

      "BA"-> 0.155 ,
      "BB"-> 0.39  ,
      "BC"-> 0.362 ,
      "BD" ->0.093 ,

      "CA"-> 0.057,
      "CB"-> 0.487 ,
      "CC"-> 0.139 ,
      "CD" ->0.317  ,

      "DA"-> 0.181,
      "DB"-> 0.373,
      "DC"-> 0.144,
      "DD" ->0.302
    )

    val emissionMatrix = Map[String, Double] (
      "AX" -> 0.302,
      "AY" -> 0.261,
      "AZ" -> 0.437,

      "BX" -> 0.34,
      "BY" -> 0.432,
      "BZ" -> 0.228,

      "CX" -> 0.483,
      "CY" -> 0.266,
      "CZ" -> 0.251,

      "DX" -> 0.419,
      "DY" -> 0.339,
      "DZ" -> 0.242

    )
    val input = "yzyxzxxyxxyxzxxyxyxyzzxxyyzyzyzzyxyyxxzxyyyzzyyxzyyzzzyzzxxzzzxxyyyzzyxxyyzxxxyxzyyxzxxyyxzxxxyxxyxx"
    val expected = "BCBCBBCBBCBCBBCBCBCBCBCBBBCBCBCBBCBBBCBCBBBBCBBCBBBCBCBCBCBCBCBCBBBCBBCBBBBCBCBCBBBCBCBBBCBCBCBCBBCB"
    val states = List("A", "B", "C", "D")
    val alphabet = List("X", "Y", "Z")
    val actual = viterbi(input)(transition, emissionMatrix, states, alphabet)
    println()
    assert(expected == actual)
  }

  "return probability of an Outcome Given a Hidden Path" should
    " be result A" in {
    val emitted = "HTHHH"
    val path = "BFBBF"

    /*
    	    x	  y	    z
      A	0.176	0.596	0.228
      B	0.225	0.572	0.203
     */

    val emissionMatrix = Map[String, Double] (
      "FH" -> 0.5,
      "FT" -> 0.5,

      "BH" -> 0.75,
      "BT" -> 0.25,

    )
    implicit val accuracy = 50
    val actual = probabilityOfOutcomeForHiddenPath(emitted, path)(emissionMatrix)
    assert(actual == 0.0000035974895474624624)
  }


}
