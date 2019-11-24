//package com.binf.tests
//
//import org.scalatest.FunSuite
//
//class AssemblyFunSpec extends FunSuite {
//
//  import com.dna.assembly.AssemblyFun._
//
//  test("Composition('TATGGGGTGC')") {
//    val dna = "TATGGGGTGC"
//    val expected = Seq("ATG", "GGG", "GGG", "GGT", "GTG", "TAT", "TGC", "TGG")
//    val actual = composition(dna, 3)
//    assert(expected === actual)
//  }
//
//  test("GenomePath") {
//    val kmers = Seq(
//      "ACCGA",
//      "CCGAA",
//      "CGAAG",
//      "GAAGC",
//      "AAGCT"
//    )
//    val res = pathToGenome(kmers)
//    assert("ACCGAAGCT" == res)
//  }
//
//  test("OverlapGraph") {
//    val input = Seq("ATGCG", "GCATG", "CATGC", "AGGCA", "GGCAT", "GGCAC")
//    val expected =
//      """GCATG -> CATGC
//        |CATGC -> ATGCG
//        |AGGCA -> GGCAT,GGCAC
//        |GGCAT -> GCATG
//      """.stripMargin
//    val res = overlapGraph(input)
//    assert(expected === res)
//  }
//
//  test("EulerianCycle") {
//    val g: Graph =
//      """0 -> 3
//        |     1 -> 0
//        |     2 -> 1,6
//        |     3 -> 2
//        |     4 -> 2
//        |     5 -> 4
//        |     6 -> 5,8
//        |     7 -> 9
//        |     8 -> 7
//        |     9 -> 6""".stripMargin
//        .replaceAll("-", "")
//        .split("\n")
////        .map(_.split("//"))
////        .map(_.head)
//        .map(_.split(Array(',', '>')))
//        .map(_.map(_.trim.toInt))
//
//    //val actual = eulerianCycle(g)
//    //val expected = "6->8->7->9->6->5->4->2->1->0->3->2->6"
//    //assert(expected === actual)
//
//  }
//
//}
