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

}
