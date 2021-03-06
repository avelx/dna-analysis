package course4

import org.scalatest._

class EvolutionaryTreeSpec extends FlatSpec {

  import course4.EvolutionaryTree._

  //  "findNearest" should "work like" in {
  //    /*
  //Data:
  //edge:       {0: [1], 1: [0]}
  //weight:     {(0, 1): 13, (1, 0): 13}
  //x: 2
  //i: 1
  //k: 0
  //Result:
  //i_near 4 k_near = 0 i_x 2 n_x 11
  //
  //*/
  //    val edge = Array(
  //      Array(1),
  //      Array(0)
  //    )
  //    var weight = Map[(Int, Int), Int]()
  //    weight = weight + ((0,1) -> 13)
  //    weight = weight + ((1,0) -> 13)
  //    val (x, i, k) = (2, 1, 0)
  //    val (i_near, k_near, i_x, n_x) = findNearest(edge, weight, x, i, k)
  //    assert(i_near === 1)
  //    assert(k_near === 0)
  //    assert(i_x == 2)
  //    assert(n_x === 11)
  //  }
  //
  //  /*
  //    [[ 0 13 21 15], [13  0 12  6], [21 12  0  6], [15  6  6  0]]
  //    2 0
  //   */
  //  "find(_ 4)" should "return" in {
  //    val matrix : Matrix = Array(
  //      Array( 0, 13, 21, 15),
  //      Array(13,  0, 12,  6),
  //      Array(21, 12,  0,  6),
  //      Array(15,  6,  6,  0)
  //    )
  //    val (i, k) = find(matrix, 4).get
  //    assert(i == 2)
  //    assert(k == 0)
  //  }
  //
  //  /*
  //  [[ 0 13 11], [13  0  2], [11  2  0]]
  //  1 0
  //   */
  //  "find" should "return" in {
  //    val matrix : Matrix = Array(
  //      Array( 0, 13, 11),
  //      Array(13,  0, 2),
  //      Array(11, 2,  0),
  //    )
  //    val (i, k) = find(matrix, 3).get
  //    assert(i == 1)
  //    assert(k == 0)
  //  }

  it should "return 2" in {
    val lines = scala.io.Source.fromFile("src/main/resources/Limb_Length.txt").getLines()
    val matrix: Matrix = lines.toArray.tail.tail
      .map(_.split(" "))
      .map(_.map(_.toInt).toArray[Int])

    assert(limbLength(25, 4, matrix) === 786)
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

  "additivePhylogeny( someMatrix, 4, 4)" should "return edges" in {
    import Matchers._
    val lines = scala.io.Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/additivePolygeny.txt").getLines()
    val data = lines.toList
    val n = data.head.toInt
    val matrix = data.tail.toArray
      .map(_.split("\t"))
      .map(_.map(_.toInt).toArray[Int])

    val (edges, weight, _) = additivePhylogeny(matrix, n, n)
    var expectedEdges = Map[Int, Array[Int]]()
    expectedEdges = expectedEdges + (0 -> Array(4))
    expectedEdges = expectedEdges + (1 -> Array(4))
    expectedEdges = expectedEdges + (4 -> Array(0, 1, 5))
    expectedEdges = expectedEdges + (2 -> Array(5))
    expectedEdges = expectedEdges + (5 -> Array(2, 3, 4))
    expectedEdges = expectedEdges + (3 -> Array(5))

    edges.foreach(edge => {
      val expEdge = expectedEdges(edge._1)
      expEdge should contain theSameElementsAs edge._2
    })
    edges.size should be equals expectedEdges.size

    var weightExpected = Map[(Int, Int), Int]()
    weightExpected = weightExpected + ((0, 4) -> 11)
    weightExpected = weightExpected + ((1, 4) -> 2)
    weightExpected = weightExpected + ((2, 5) -> 6)
    weightExpected = weightExpected + ((3, 5) -> 7)
    weightExpected = weightExpected + ((4, 0) -> 11)
    weightExpected = weightExpected + ((4, 5) -> 4)
    weightExpected = weightExpected + ((4, 1) -> 2)
    weightExpected = weightExpected + ((5, 4) -> 4)
    weightExpected = weightExpected + ((5, 3) -> 7)
    weightExpected = weightExpected + ((5, 2) -> 6)
    weight.toList should contain theSameElementsAs weightExpected.toList

  }

  "argmin" should "return " in {
    //    val d = Array[Array[Int]](
    //      Array(-6, -6, 5),
    //      Array(-5, 8, 9)
    //    )
    val d = Array[Array[Int]](
      Array(0, 13, 21, 22),
      Array(13, 0, 12, 13),
      Array(21, 12, 0, 13),
      Array(22, 13, 13, 0)
    )
    val D: Array[Array[Float]] = d.map(_.map(_.toFloat))
    val index = argmin_S(D)
    assert(index === 6)
  }


  "UPGMA" should "return next result" in {
    val path = "/Users/pavel/Sources/dna-analysis/src/main/resources/upgma.txt"
    val lines = scala.io.Source.fromFile(path).getLines()
    val data = lines.toList
    val n = data.head.toInt

    val matrix = data.tail.toArray
      .map(_.split("\t"))
      .map(_.map(_.toInt).toArray[Int])

    val expected =
      """
        |0->5:7.000
        |1->6:8.833
        |2->4:5.000
        |3->4:5.000
        |4->2:5.000
        |4->3:5.000
        |4->5:2.000
        |5->0:7.000
        |5->4:2.000
        |5->6:1.833
        |6->5:1.833
        |6->1:8.833
        |""".stripMargin

    val result = upgma(matrix, n)
  }

//  "smallParsimony" should "return next result" in {
//    val in =
//      """|4
//         |4->CAAATCCC
//         |4->ATTGCGAC
//         |5->CTGCGCTG
//         |5->ATGGACGA
//         |6->4
//         |6->5
//         |""".stripMargin.split("\n")
//    val result = smallParsimony(in)
//    val expected = 5
//    //assert(result === expected)
//  }
}