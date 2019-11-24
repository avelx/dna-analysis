//package com.binf.tests
//
//import org.scalatest.FunSuite
//
//class FunBaseSpec extends FunSuite {
//
//  import com.binf.Fun._
//
//  test("AA") {
//    implicit val k: Int = 2
//    assert(patternToNumber("AA") == 0)
//  }
//
//  test("TT") {
//    implicit val k: Int = 2
//    assert(patternToNumber("TT") == 15)
//  }
//
//  test("CC") {
//    implicit val k: Int = 2
//    assert(patternToNumber("CC") == 5)
//  }
//
//  test("ATGCAA") {
//    implicit val k: Int = 6
//    assert(patternToNumber("ATGCAA") == 912)
//  }
//
//  test("ACGCGGCTCTGAAA") {
//    implicit val k: Int = 2
//    assert(computingFreq("ACGCGGCTCTGAAA") sameElements Array(2, 1, 0, 0, 0, 0, 2, 2, 1, 2, 1, 0, 0, 1, 1, 0))
//  }
//
//  test("numberToPattern('AGTC')") {
//    implicit val k: Int = 4
//    assert(numberToPattern(45) == "AGTC")
//  }
//
//  test("reverse complement of AAAACCCGGT") {
//    assert(reverseComplement("AAAACCCGGT") == "ACCGGGTTTT")
//  }
//
//  test("pattern match for ATAT in GATATATGCATATACTT") {
//    assert(patternIndecs("ATAT", "GATATATGCATATACTT") == List(1, 3, 9))
//  }
//
//  test("ClumpFunding") {
//    val genome = "CGGACTCGACAGATGTGAAGAACGACAATGTGAAGACTCGACACGACAGAGTGAAGAGAAGAGGAAACATTGTAA"
//    assert(clumpFinding(genome, 5, 50, 4) == List("CGACA", "GAAGA"))
//  }
//
//  test("skew(CATGGGCATCGGCCATACGCC)") {
//    assert(skew("CATGGGCATCGGCCATACGCC") == List(0, -1, -1, -1, 0, 1, 2, 1, 1, 1, 0, 1, 2, 1, 0, 0, 0, 0, -1, 0, -1, -2))
//  }
//
//  test("skewMin(TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT)") {
//    assert(skewMin("TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT") == List(11, 24))
//  }
//
//  test("hammingDistance('GGGCCGTTGGT', 'GGACCGTTGAC')") {
//    assert(hammingDistance("GGGCCGTTGGT", "GGACCGTTGAC") == 3)
//  }
//
//  test("approximateOccurrences") {
//    val pattern = "ATTCTGGA"
//    val genome = "CGCCCGAATCCAGAACGCATTCCCATATTTCGGGACCACTGGCCTCCACGGTACGGACGTCAATCAAAT"
//    assert(approximateOccurrences(genome, pattern, 3) == Seq(6, 7, 26, 27))
//  }
//
//  test("Profiler test") {
//    val motifs = Seq(
//      "GTCG",
//      "GCTG",
//      "GCCT",
//      "CCCG",
//      "GGCG"
//    )
//    val res = profiler(motifs)
//    val expected = Array(
//      Array(0.0, 0.0, 0.0, 0.0),
//      Array(0.2, 0.6, 0.8, 0.0),
//      Array(0.8, 0.2, 0.0, 0.8),
//      Array(0.0, 0.2, 0.2, 0.2)
//    )
//    assert(res === expected)
//  }
//
//  test("Motifer") {
//    val matrix = Array(
//      Array(0.8, 0.0, 0.0, 0.2),
//        Array(0.0, 0.6, 0.2, 0.0),
//        Array(0.2, 0.2, 0.8, 0.0),
//        Array(0.0, 0.2, 0.0, 0.8)
//      )
//    val dna = Seq(
//      "ttaccttaac",
//      "gatgtctgtc",
//      "acggcgttag",
//      "ccctaacgag",
//      "cgtcgagaggt"
//    )
//
//    val expected = Seq(
//      "acct",
//      "atgt",
//      "gcgt",
//      "acga",
//      "aggt"
//    ).map(_.toUpperCase())
//
//    val res = dna.map(d => profileMostProbableKmer(d.toUpperCase(), 4, matrix))
//
//    assert(res === expected)
//  }
//
//  test("count motifs"){
//    val dna = Seq(
//      "ACCT",
//      "ATGT"
//    )
//
//  }
//
//  test("Consensus string") {
//    val dna = Seq(
//      "ttaccttaac",
//      "gatgtctgtc",
//      "acggcgttag",
//      "ccctaacgag",
//      "cgtcgagaggt"
//    ).map(_.toUpperCase())
//
//    val expected = "CCTCCATAAG".toUpperCase
//    val res = consensusString(dna)
//    assert(res == expected)
//  }
//
//}
//
