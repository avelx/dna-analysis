package com.binf.tests

import org.scalatest.FunSuite

class AssemblyFunSpec extends FunSuite {

  import com.dna.assembly.AssemblyFun._

  test("Composition('TATGGGGTGC')") {
    val dna = "TATGGGGTGC"
    val expected = Seq("ATG", "GGG", "GGG", "GGT", "GTG", "TAT", "TGC", "TGG")
    val actual = composition(dna, 3)
    assert(expected === actual)
  }

  test("GenomePath") {
    val kmers = Seq(
      "ACCGA",
      "CCGAA",
      "CGAAG",
      "GAAGC",
      "AAGCT"
    )
    val res = pathToGenome(kmers)
    assert("ACCGAAGCT" == res)
  }

}
