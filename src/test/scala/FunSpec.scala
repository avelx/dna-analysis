package com.binf.tests

import org.scalatest.FunSuite

class FunBaseSpec extends FunSuite {

  import com.binf.Fun._

  test("AA") {
    implicit val k: Int = 2
    assert(patternToNumber("AA") == 0)
  }

  test("TT") {
    implicit val k: Int = 2
    assert(patternToNumber("TT") == 15)
  }

  test("CC") {
    implicit val k: Int = 2
    assert(patternToNumber("CC") == 5)
  }

  test("ATGCAA") {
    implicit val k: Int = 6
    assert( patternToNumber("ATGCAA") == 912 )
  }

  test("ACGCGGCTCTGAAA") {
    implicit val k: Int = 2
    assert( computingFreq("ACGCGGCTCTGAAA") sameElements  Array(2, 1, 0, 0, 0, 0, 2, 2, 1, 2, 1, 0 ,0, 1, 1, 0) )
  }


}

