package course6

object HmmApp extends App {
  import HMM._

  val transition = Map[String, Double] (
    "AA" -> 0.345,
    "AB" -> 0.655,
    "BA"-> 0.409,
    "BB"-> 0.591
  )

  val path = "ABAABABAABBBBBBBAAABABBABABBBBBBBAAABAAABAABBAABAB"

  val res = probabilityOfHiddenPath(path)(transition)
  println(res)
}
