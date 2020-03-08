package course5

import course5.Cluster._

import scala.util.Try

object Runner extends App {

  val lines = scala.io.Source.fromFile("/Users/pavel/devcore/sources/dna-analysis/src/main/resources/data/distortion.txt")
    .getLines().toList

  val (k, m) = {
    val r = lines.head.split(" ").map(_.toInt)
    (r(0), r(1) )
  }

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

  val result = distortion(data, centers)
  println(result)

  //val acutal = farthesFirstTraversal(k, m, in)
  //acutal.foreach(p => {
  //  println(p.dimPos.mkString(" "))
  //})

}
