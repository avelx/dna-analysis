package course6

object HmmApp extends App {
  import HMM._

  val transition = Map[String, Double] (
    "AA" -> 0.634,
    "AB" -> 0.387,
//    "AC" -> 0.34,
//    "AD" -> 0.34,

    "BA"-> 0.366,
    "BB"-> 0.613
//    "BC"-> 0.074,
//    "BD" -> 0.34,

//    "CA"-> 0.309,
//    "CB"-> 0.324,
//    "CA"-> 0.367,
//    "AC" -> 0.34,
  )

  val emissionMatrix = Map[String, Double] (
    "AX" -> 0.532,
    "AY" -> 0.226,
    "AZ" -> 0.241,

    "BX" -> 0.457,
    "BY" -> 0.192,
    "BZ" -> 0.351

//    "CX" -> 0.39,
//    "CY" -> 0.186,
//    "CZ" -> 0.424

  )
  val input = "zxxxxyzzxyxyxyzxzzxzzzyzzxxxzxxyyyzxyxzyxyxyzyyyyzzyyyyzzxzxzyzzzzyxzxxxyxxxxyyzyyzyyyxzzzzyzxyzzyyy"
  val expected = "AAAAAAAAAAAAAABBBBBBBBBBBAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBAAA"
  val states = List("A", "B")

  val actual = viterbi(input)(transition, emissionMatrix, states)
  println()
  println(actual)
  println(expected)
  println(actual == expected)
}
