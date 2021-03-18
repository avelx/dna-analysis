package course6

object HmmApp extends App {
  import HMM._

  // Enter left to right
  val transition = Map[String, Double] (
    // + -
    "A+A-"-> 0.180,
    "A+C-" -> 0.268,
    "A+G-" -> 0.430,
    "A+T-" -> 0.122,

    "C+A-"-> 0.191,
    "C+C-" -> 0.299,
    "C+G-" -> 0.299,
    "C+T-" -> 0.211,

    "G+A-"-> 0.161,
    "G+C-" -> 0.346,
    "G+G-" -> 0.373,
    "G+T-" -> 0.120,

    "T+A-"-> 0.082,
    "T+C-" -> 0.357,
    "T+G-" -> 0.391,
    "T+T-" -> 0.170,

    // - +
    "A-A+"-> 0.300,
    "A-C+" -> 0.200,
    "A-G+" -> 0.290,
    "A-T+" -> 0.210,

    "C-A+"-> 0.319,
    "C-C+" -> 0.300,
    "C-G+" -> 0.081,
    "C-T+" -> 0.300,

    "G-A+"-> 0.251,
    "G-C+" -> 0.251,
    "G-G+" -> 0.299,
    "G-T+" -> 0.199,

    "T-A+"-> 0.176,
    "T-C+" -> 0.242,
    "T-G+" -> 0.291,
    "T-T+" -> 0.291,
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
    "A+"-> 0.125,
    "C+" -> 0.125,
    "G+" -> 0.125,
    "T+" -> 0.125,
    "A-" -> 0.125,
    "C-" -> 0.125,
    "G-" -> 0.125,
    "T-" -> 0.125
  )

  implicit val debug = true

  val input = "ATGGCCCGAACCAAGCAGACTGCGCGCAAGTCAACGGGTGGCAAGGCGCCGCGCAAGCAGCTGGCCACCAAGGTGGCTCGCAAGAGCGCACCTGCCACTGGCGGCGTGAAGAAGCCGCACCGCTACCGGCCCGGCACGGTGGCGCTTCGCGAGATCCGCCGCTACCAGAAGTCCACTGAGCTGCTAATCCGCAAGTTGCCCTTCCAGCGGCTGATGCGCGAGATCGCTCAGGACTTTAAGACCGACCTGCGCTTCCAGAGCTCGGCCGTGATGGCGCTGCAGGAGGCGTGCGAGTCTTACCTGGTGGGGCTGTTTGAGGACACCAACCTGTGTGTCATCCATGCCAAACGGGTCACCATCATGCCTAAGGACATCCAGCTGGCACGCCGTATCCGCGGGGAGCGGGCCTAGGAGGGCTATCTCGCCACCTGAGAGGTTGCGCAACGTTCACCCCAAAGGCTCTTTTAAGAGCCACCCACCT"

  val expected = "FFF"
  val states = List("A+", "C+", "G+", "T+", "A-", "C-", "G-", "T-")
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
