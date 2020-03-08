package course5

import org.scalatest._
import org.scalactic._
import scala.util.Try

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

    val lines = scala.io.Source.fromFile("/src/main/resources/data/farthestfisttraversal.txt")
      .getLines().toList
    val (k, m) = {
      val r = lines.head.split(" ").map(_.toInt)
      (r(0), r(1) )
    }
    val in = lines.tail
      .map( _.split(" ") )
      .map( row => Point(row.map(_.toDouble)) )

    val acutal = farthesFirstTraversal(k, m, in)
    acutal.foreach(p => {
      println(p)
    })

    val expected = Seq(
      Point( Seq(0.0d, 0.0d) ),
      Point( Seq(5.0d, 5.0d) ),
      Point( Seq(0.0d, 5.0d) )
    )
    assert(acutal === expected)
  }

  "Compute distortion for ..." should " return 18.24556" in {
    val lines = scala.io.Source.fromFile("/Users/pavel/devcore/sources/dna-analysis/src/main/resources/data/distortion.txt")
      .getLines().toList

    var isCenter = true
    val in = lines.tail
      .map( _.split(" ") )
      .map( row => Try { Point( row.toSeq.map(_.toDouble)) }.toOption )
      .map(e => e match {
        case Some(p) => (Some(p), isCenter)
        case None => {
          isCenter = !isCenter
          (None, isCenter)
        }
      }).filter(e => e._1.isDefined)
    val (centers, data) = {
      val (a, b) = in.partition(e => e._2 == true)
      (a.map(_._1.get), b.map(_._1.get))
    }

    implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(0.01)
    val actual = distortion(data, centers)
    val expected : Double = 18.246
    assert(actual === expected)
  }
}
