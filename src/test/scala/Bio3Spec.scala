package com.dna.compare.genes.tests

import com.dna.compare.genes.Bio3
import org.scalatest.FunSuite

class Bio3Spec extends FunSuite {

  test("Empty strings") {
    val a = ""
    val b = ""
    val result = Bio3.LCS(a, b)
    assert(result === None)
  }

}