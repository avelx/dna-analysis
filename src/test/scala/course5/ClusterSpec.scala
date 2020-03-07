package course5

import org.scalatest._

class ClusterSpec extends FlatSpec {

  import Cluster._
  "Input:" +
    "(1, 1)" +
    "(2, 4)" +
    "(4, 2) expected result" should  "be X.YY" in {
    val v = Point( Seq(1, 2, 4) )
    val w = Point( Seq(1, 4, 2) )
    val actual = d(v, w)
    val expected : Double = 2.8284271247461903
    assert(expected === actual)
  }

  "Input: points and center" should "have next result" in {
    val data = Seq( Point( Seq(1, 6) ), Point( Seq(1, 3) ), Point( Seq(3, 4) ), Point( Seq(5, 6) ),
      Point( Seq(5, 2) ), Point( Seq(8, 7) ), Point( Seq(7, 1) ), Point( Seq(10, 3) ) )
    val centers = Seq( Point( Seq(2, 4) ), Point( Seq(6, 7) ), Point( Seq(7, 3) ) )
    val expected = 3.0
    val actual = maxDistance(data, centers)._1
    println(actual)
    assert(expected === actual)
  }

  """Input 3 2
    |0.0 0.0
    |5.0 5.0
    |0.0 5.0
    |1.0 1.0
    |2.0 2.0
    |3.0 3.0
    |1.0 2.0
    |""".stripMargin should """return
      |0.0 0.0
      |5.0 5.0
      |0.0 5.0
      |""".stripMargin in {
    val (k, m) = (3, 2)
    val in : Seq[Point] = Seq(
      Point( Seq(0.0d, 0.0d) ), Point( Seq(5.0d, 5.0d) ),
      Point( Seq(0.0d, 5.0d) ), Point( Seq(1.0d, 1.0d) ),
      Point( Seq(2.0d, 2.0d) ), Point( Seq(3.0d, 3.0d) ),
      Point( Seq(1.0d, 2.0d) )
    )
    val acutal = farthesFirstTraversal(k, m, in)
    val expected = Seq(
      Point( Seq(0.0d, 0.0d) ),
      Point( Seq(5.0d, 5.0d) ),
      Point( Seq(0.0d, 5.0d) )
    )
    assert(acutal === expected)
  }

}
