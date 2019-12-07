package com.avel.dna

import org.scalatest._

class EvolutionaryTreeSpec extends FlatSpec {

  import course4.EvolutionaryTree._

  it should "return 2" in {
    val lines = scala.io.Source.fromFile("src/main/resources/Limb_Length.txt").getLines()
    val matrix: Matrix = lines.toArray.tail.tail
      .map(_.split(" "))
      .map(_.map(_.toInt).toArray[Int])

    assert( limbLength(25, 4, matrix) === 786)
  }

  it should "return next result" in {

    val input =
      """
        |4
        |0->4:11
        |1->4:2
        |2->5:6
        |3->5:7
        |4->0:11
        |4->1:2
        |4->5:4
        |5->4:4
        |5->3:7
        |5->2:6
        |""".stripMargin

    val result = leafDistance(input)
    val expected: Matrix = Array(
      Array(0, 13, 21, 22),
      Array(13, 0, 12, 13),
      Array(21, 12, 0, 13),
      Array(22, 13, 13, 0)
    )

    assert(result === expected)
  }

}