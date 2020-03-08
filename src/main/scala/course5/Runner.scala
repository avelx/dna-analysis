package course5

import course5.Cluster.{Point, farthesFirstTraversal}

object Runner extends App {

  val lines = scala.io.Source.fromFile("/Users/pavel/devcore/sources/dna-analysis/src/main/resources/data/farthestfisttraversal.txt")
    .getLines().toList

  val (k, m) = {
    val r = lines.head.split(" ").map(_.toInt)
    (r(0), r(1) )
  }
  val in = lines.tail
    .map( _.split(" ") )
    .map( row => Point( row.toSeq.map(_.toDouble)) )

  val acutal = farthesFirstTraversal(k, m, in)
  acutal.foreach(p => {
    println(p.dimPos.mkString(" "))
  })

}
