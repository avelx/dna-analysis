package course6

object HmmApp extends App {
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

  val expected = "FFF"
  val states = List("E", "D1", "D2", "I", "A1", "A2")
  val alphabet = List("A", "C", "G", "T")
  val actual = viterbi2(input)(transition, emissionMatrix, states, startProbabilities, alphabet)

  val res = actual._2
  println(res)
  println( res.filter(c => c == 'I').length)
  //println()
  //println(actual._2)
  //println(expected)
  //println(expected.length)
  //println(actual._2 == expected)
}
