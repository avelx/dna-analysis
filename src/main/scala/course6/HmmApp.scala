package course6

object HmmApp extends App {
  import HMM._

  // Enter left to right
  val transition = Map[String, Double] (
    "FF" -> 0.533,
    "FB" -> 0.466,
    "BF" -> 0.60,
    "BB"-> 0.40
  )

  val emissionMatrix = Map[String, Double] (
    "FH"-> 0.857,
    "FT" -> 0.533,
    "BH" -> 0.142,
    "BT" -> 0.466
  )

  val startProbabilities = Map[String, Double] (
    "F"-> 0.5,
    "B" -> 0.5
  )

  implicit val debug = true

  val input = "HTH"

  val expected = "FFF"
  val states = List("F", "B")
  val alphabet = List("H", "T")
  val actual = viterbi2(input)(transition, emissionMatrix, states, startProbabilities, alphabet)

  println()
  println(actual._2)
  println(expected)
  println(expected.length)

  println(actual._2 == expected)
}
