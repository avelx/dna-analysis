package course4

import scala.collection.mutable.ListBuffer

object SiteCounter extends App {
  println("Site Counter App ...")

  val data = scala.io.Source.fromFile("/Users/pavel/Desktop/PhyloAnalysis-CDF.csv").getLines().toList

  // println(data.length)
  // KJ660348: 1 .. 215
  // KC589025: 216 .. 430

  val a = data.slice(2, 214).mkString("").replace(",", "")
  val b = data.slice(217, 430).mkString("").replace(",", "")

  println(a)
  println(b)



}
