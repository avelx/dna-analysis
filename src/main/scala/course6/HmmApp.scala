package course6

object HmmApp extends App {
  import HMM._

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

  val actual = viterbi(input)(transition, emissionMatrix, states)
  println()
  println(actual)
  println(expected)
  println(actual.takeRight(40) == expected.takeRight(40))
}
