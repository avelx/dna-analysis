package course6

object HmmApp extends App {
  import HMM._

  // Enter left to right
  val transition = Map[String, Double] (
    "AA" -> 0.425,
    "AB" -> 0.285,
    "AC" -> 0.29,
//    "AD" -> 0.018,

    "BA"-> 0.452,
    "BB"-> 0.438 ,
    "BC"-> 0.11 ,
//    "BD" ->0.339 ,

    "CA"-> 0.046,
    "CB"-> 0.576 ,
    "CC"-> 0.378,
//    "CD" ->0.214  ,

//  "DA"-> 0.29,
//  "DB"-> 0.344,
//  "DC"-> 0.173
//  "DD" ->0.193
  )

  val emissionMatrix = Map[String, Double] (
    "AX" -> 0.38,
    "AY" -> 0.154,
    "AZ" -> 0.466,

    "BX" -> 0.54,
    "BY" -> 0.401,
    "BZ" -> 0.059,

    "CX" -> 0.437,
    "CY" -> 0.206,
    "CZ" -> 0.357

//  "DX" -> 0.205,
//  "DY" -> 0.564,
//  "DZ" -> 0.231

  )
  val input = "yzxxxyyxyzyyyxzyxzyxzxxxxzyzxxxzzxzyxxzzyzzzxyzzzyyzzxzxxxzxyyyxzyxzzzxyyzxzyzyxxyzzyzyyzyzyyzzyxxyz"
  val expected = ""
  val states = List("A", "B", "C") //, "D")
  val alphabet = List("X", "Y", "Z")
  val actual = viterbi(input)(transition, emissionMatrix, states, alphabet)
  println()
  println(actual)
  println(expected)
  println(expected.length)

  val right = 99
  println(actual.takeRight(right) == expected.takeRight(right))
  println(actual == expected)
}
