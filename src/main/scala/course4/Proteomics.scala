package course4

import course4.ProteomicsRunner.input

object ProteomicsRunner extends App {
  import Proteomics._

  val inputText = "57 71 154 185 301 332 415 429 486"
  //val inputText = "113 129 269 315 406 414 534 543 640 663 739 776 840 863 897 976 1025 1047 1138 1146 1235 1243 1334 1356 1405 1484 1518 1541 1605 1642 1718 1741 1838 1847 1967 1975 2066 2112 2252 2268 2381"
  val input = inputText.split(" ").map(_.toInt).toList


  val graph = graphSpectrum(input)
  graph.foreach(println)
}

object Proteomics {

  val integer_mass_table : Map[Int, String] = {
    val data = scala.io.Source
      .fromFile("src/main/resources/integer_mass_table.txt")
      .getLines().toList.map(_.split(" "))
     data.map(_(1).toInt).zip( data.map(_(0) ) ).toMap
  }

  def convolution(a: Array[Int]): Array[(Int, Int)] = {
    val matrix = for {
      x <- a
      y <- a
      e = x - y
      if (e > 0)
    } yield (y, x)

    matrix.toArray
  }

  def graphSpectrum(in: List[Int]): List[(Int, Int)] = {
    val data = for {
      x <- 0 :: in
      y <- 0 :: in
      z = Math.abs(x - y)
      if integer_mass_table.contains(z)
    } yield {
      if (x < y) (s"$x->$y", s"${integer_mass_table(z)}")
      else (s"$y->$x", s"${integer_mass_table(z)}")
    }

    data.toMap
      .map(r => r._1.split("->") )
      .map(r => r.map(_.toInt) )
      .toList.map(r => ( r(0), r(1) ) )
  }

  def decodingIdealSpectrum(spectrum: List[Int]) : String = {
    ""
  }

}
