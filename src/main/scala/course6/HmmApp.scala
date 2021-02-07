package course6

object HmmApp extends App {
  import HMM._

//  val transition = Map[String, Double] (
//    "AA" -> 0.345,
//    "AB" -> 0.655,
//    "BA"-> 0.409,
//    "BB"-> 0.591
//  )
//
//  val path = "ABAABABAABBBBBBBAAABABBABABBBBBBBAAABAAABAABBAABAB"
//
//  val res = probabilityOfHiddenPath(path)(transition)
//  println(res)

  val emitted = "yxzyyxyzyyxyyyxxxxxxyyzxyyyzzxxxxyzyyxxxxzyzxxyzzy"
  val path = "ABBBBAAAABBAAAABBAABBABAAABAABAABABABAABBAAABABAAB"

  /*
	x	y	z
A	0.116	0.838	0.046
B	0.359	0.13	0.511

   */

  val emissionMatrix = Map[String, Double] (
    "AX" -> 0.116		,
    "AY" -> 0.838,
    "AZ" -> 0.046,

    "BX" -> 0.359	,
    "BY" -> 	0.13,
    "BZ" -> 0.511

  )
  implicit val accuracy = 50
  val res = probabilityOfOutcomeForHiddenPath(emitted, path)(emissionMatrix)
  println(res)
}
