package com.avel.dna

import org.scalatest._

class EvolutionaryTreeSpec extends FlatSpec {

  import course4.EvolutionaryTree._

  "findNearest" should "work like" in {
    /*
Data:
edge:       {0: [1], 1: [0]}
weight:     {(0, 1): 13, (1, 0): 13}
x: 2
i: 1
k: 0
Result:
i_near 4 k_near = 0 i_x 2 n_x 11

*/
    val edge = Array(
      Array(1),
      Array(0)
    )
    val weight = Array.fill(2)( Array.fill(2)(0) )
    weight(0)(1) = 13
    weight(1)(0) = 13
    val (x, i, k) = (2, 1, 0)
    val (i_near, k_near, i_x, n_x) = findNearest(edge, weight, x, i, k)
    assert(i_near === 1)
    assert(k_near === 0)
    assert(i_x == 2)
    assert(n_x === 11)
  }

  /*
    [[ 0 13 21 15], [13  0 12  6], [21 12  0  6], [15  6  6  0]]
    2 0
   */
  "find(_ 4)" should "return" in {
    val matrix : Matrix = Array(
      Array( 0, 13, 21, 15),
      Array(13,  0, 12,  6),
      Array(21, 12,  0,  6),
      Array(15,  6,  6,  0)
    )
    val (i, k) = find(matrix, 4).get
    assert(i == 2)
    assert(k == 0)
  }

  /*
  [[ 0 13 11], [13  0  2], [11  2  0]]
  1 0
   */
  "find" should "return" in {
    val matrix : Matrix = Array(
      Array( 0, 13, 11),
      Array(13,  0, 2),
      Array(11, 2,  0),
    )
    val (i, k) = find(matrix, 3).get
    assert(i == 1)
    assert(k == 0)
  }

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

  "additivePhylogeny(_, _)" should "return edges" in {
    val matrix : Matrix = Array(
      Array(0,	13,	21,	22),
      Array(13,	0,	12,	13),
      Array(21,	12,	0, 13),
      Array(22,	13,	13,	0)
    )
    val (edges, weight, _) = additivePhylogeny(matrix, 4, 4)
    edges.foreach(edge => println( edge.mkString(" ") ) )
  }

}