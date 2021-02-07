package course6

import org.scalatest.FlatSpec

class HMMSpec extends FlatSpec {
  import HMM.probabilityOfHiddenPath

  "return correct probability of Hidden Path" should " be " in {
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

}
