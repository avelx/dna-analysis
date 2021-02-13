package course6

object HmmApp extends App {
  import HMM._

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
  val input = "xyxzzxyxyy"
  val expected = "AAABBAAAAA"
  val states = List("A", "B")
  val actual = viterby(input)(transition, emissionMatrix, states)

  println(actual)
}
