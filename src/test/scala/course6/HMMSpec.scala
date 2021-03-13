package course6

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

  "application chalange - A" should " be as .. A" in {
    import HMM._

    // Enter left to right
    val transition = Map[String, Double] (
      "EE" -> 0.85,
      "ED1" -> 0.15,
      "ED2"-> 0.0,
      "EI"-> 0.0,
      "EA1" -> 0.0,
      "EA2"-> 0.0,

      "D1E" -> 0.0,
      "D1D1" -> 0.0,
      "D1D2"-> 1.0,
      "D1I"-> 0.0,
      "D1A1" -> 0.0,
      "D1A2"-> 0.0,

      "D2E" -> 0.0,
      "D2D1" -> 0.0,
      "D2D2"-> 0.0,
      "D2I"-> 1.0,
      "D2A1" -> 0.0,
      "D2A2"-> 0.0,

      "IE" -> 0.0,
      "ID1" -> 0.0,
      "ID2"-> 0.0,
      "II"-> 0.9,
      "IA1" -> 0.1,
      "IA2"-> 0.0,

      "A1E" -> 0.0,
      "A1D1" -> 0.0,
      "A1D2"-> 0.0,
      "A1I"-> 0.0,
      "A1A1" -> 0.0,
      "A1A2"-> 1.0,

      "A2E" -> 1.0,
      "A2D1" -> 0.0,
      "A2D2"-> 0.0,
      "A2I"-> 0.0,
      "A2A1" -> 0.0,
      "A2A2"-> 0.0

    )

    val emissionMatrix = Map[String, Double] (
      "EA"-> 0.25,
      "EC" -> 0.25,
      "EG"-> 0.25,
      "ET" -> 0.25,

      "D1A" -> 0.05,
      "D1C" -> 0.00,
      "D1G" -> 0.95,
      "D1T" -> 0.00,

      "D2A" -> 0.05,
      "D2C" -> 0.00,
      "D2G" -> 0.00,
      "D2T" -> 0.95,

      "IA" -> 0.40,
      "IC" -> 0.10,
      "IG" -> 0.10,
      "IT" -> 0.40,

      "A1A" -> 0.95,
      "A1C" -> 0.00,
      "A1G" -> 0.05,
      "A1T" -> 0.00,

      "A2A" -> 0.05,
      "A2C" -> 0.00,
      "A2G" -> 0.95,
      "A2T" -> 0.00
    )

    val startProbabilities = Map[String, Double] (
      "E"-> 0.5,
      "D1" -> 0.0,
      "D2" -> 0.0,
      "I" -> 0.5,
      "A1" -> 0.0,
      "A2" -> 0.0
    )

    implicit val debug = true

    val input = "ATGGCCCGAACCAAGCAGACTGCGCGCAAGTCAACGGGTGGCAAGGCGCCGCGCAAGCAGCTGGCCACCAAGGTGGCTCGCAAGAGCGCACCTGCCACTGGCGGCGTGAAGAAGCCGCACCGCTACCGGCCCGGCACGGTGGCGCTTCGCGAGATCCGCCGCTACCAGAAGTCCACTGAGCTGCTAATCCGCAAGTTGCCCTTCCAGCGGCTGATGCGCGAGATCGCTCAGGACTTTAAGACCGACCTGCGCTTCCAGAGCTCGGCCGTGATGGCGCTGCAGGAGGCGTGCGAGTCTTACCTGGTGGGGCTGTTTGAGGACACCAACCTGTGTGTCATCCATGCCAAACGGGTCACCATCATGCCTAAGGACATCCAGCTGGCACGCCGTATCCGCGGGGAGCGGGCCTAGGAGGGCTATCTCGCCACCTGAGAGGTTGCGCAACGTTCACCCCAAAGGCTCTTTTAAGAGCCACCCACCT"

    val expected = "EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEED1D2IIIIA1A2EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEED1D2IIIIIA1A2EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEED1D2IIIIIA1A2EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEED1D2IIIA1A2EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEED1D2IIIIIIIIIIIIIIIIIIIIA1A2EEEEEEEEEEEE"

    val states = List("E", "D1", "D2", "I", "A1", "A2")
    val alphabet = List("A", "C", "G", "T")
    val actual = viterbi2(input)(transition, emissionMatrix, states, startProbabilities, alphabet)
    assert(actual._2 == expected)
  }

}
