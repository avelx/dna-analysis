//package com.bio3.test
//
//import com.bio3.DynamicProg.Matrix
//import org.scalatest._
//
//class ManhattanTouristSpec extends FlatSpec with Matchers {
//
//  "Long path" should " 44" in {
//    import com.bio3.DynamicProg.manhattanTourist
//    import com.bio3.StringTransformers._
//
//    val n = 4
//    val m = 4
//    val down: Matrix =
//      """
//        |1 0 2 4 3
//        |4 6 5 2 1
//        |4 4 5 2 1
//        |5 6 8 5 3
//        |""".stripMargin
//
//
//    val right: Matrix =
//      """
//        |3 2 4 0
//        |3 2 4 2
//        |0 7 3 3
//        |3 3 0 2
//        |1 3 2 2
//        |""".stripMargin
//
//    assert(manhattanTourist(n, m, down, right) === 34)
//
//  }
//
//}
