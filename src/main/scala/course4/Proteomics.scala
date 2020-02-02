package course4

import scala.util.Try

object ProteomicsRunner extends App {
  import Proteomics._

  val spectralVector = "0 0 0 4 -2 -3 -1 -7 6 5 3 2 1 9 3 -8 0 3 1 2 1 8"
    .split(" ").map(_.toInt)
  val proteome = "XZZXZXXXZXZZXZXXZ"
  val integer_mass : Map[String, Int] = Map("X" -> 4, "Z" -> 5)

  val cands = for {
    i <- 0 to proteome.length - 1
    j <- i to proteome.length - 1
    candidate = proteome.substring(i, j + 1)
    candidateScore = candidate.map(c => integer_mass(c.toString)).sum
    if candidateScore <= spectralVector.length
    prefixesMasses = ( getPrefixes(candidate) :+ candidate )
      .map(r => r.map(c => integer_mass(c.toString) ).sum )
     currScore = prefixesMasses.map(m => spectralVector(m - 1) ).sum
  } yield  ( candidate, currScore)

  cands.foreach(println)
  println( cands.maxBy(_._2) )
}

object Proteomics {

  val integer_mass_table: Map[Int, String] = {
    val data = scala.io.Source
      .fromFile("src/main/resources/integer_mass_table.txt")
      .getLines().toList.map(_.split(" "))
    data.map(_ (1).toInt).zip(data.map(_ (0))).toMap
  }

  val integer_mass_table_revers: Map[String, Int] = {
    val data = scala.io.Source
      .fromFile("src/main/resources/integer_mass_table.txt")
      .getLines().toList.map(_.split(" "))

    data.map(r => r(0)).zip(data.map(r => r(1).toInt)).toMap
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
      .map(r => r._1.split("->"))
      .map(r => r.map(_.toInt))
      .toList.map(r => (r(0), r(1)))
  }

  def getPrefixes(peptide: String): List[String] = {
    for {
      i <- 1 to peptide.length - 1
      p = peptide.substring(0, i)
    } yield p
  }.toList

  def getSuffixes(peptide: String) : List[String] = {
    for {
      i <- 0 to peptide.length - 2
      p = peptide.substring(peptide.length - 1 - i, peptide.length)
    } yield p
  }.toList

  def peptideToSpectrum(peptide: String) : List[Int] = {
    val prefixAndSuffix = getPrefixes(peptide) ++ getSuffixes(peptide)
    (prefixAndSuffix :+ peptide).map(row => row.map(c => integer_mass_table_revers(c.toString) ).sum ).sorted
  }

  def decodingIdealSpectrum(spectrum: List[Int]) : String = {
    val graph = graphSpectrum(spectrum)

    def getPaths(s: Int, acc: List[Int]) : List[List[Int]] = graph.filter(e => e._1 == s) match {
      case h::tail => getPaths(h._2, h._1 :: acc)  ++ tail.map(q => getPaths(q._2, q._1 ::acc)).flatten
      case Nil => List(s :: acc)
    }

    val result = getPaths(spectrum.head, List(0) ).map(r => r.reverse)
    val peptides = result.map(row => {
      (0 to row.length - 2).map(i => integer_mass_table(row(i + 1) - row(i)) )
    }).map(_.mkString(""))

    //peptides.foreach(r => println(r.mkString("") ) )
    peptides.find(peptide => peptideToSpectrum( peptide) == spectrum) match {
      case Some(p) => {
        println(p)
        p
      }
      case None => throw new Error("No peptide found for ideal spectrum")
    }

  }

  def getPeptideVector(peptide: String)(integer_mass: Map[String, Int] ): String = {
    val prefixes = getPrefixes(peptide) :+ peptide

    val positions = prefixes.map(r => {
      r.map(c => integer_mass(c.toString)).sum
    })

    val z  = Array.fill(positions.max)(0)
    positions.map(i => z(i - 1) = 1  )
    z.mkString(" ")
  }

  def peptideVectorToPeptide(vector: String)(integer_mass_revers: Map[Int, String] ) : String = {
    val positions = vector.split(" ").zipWithIndex.map(p => if (p._1.equals("1")) p._2 + 1 else 0 ).filter(e => e > 0)
    val result = for {
      i <- 1 to positions.length - 1
      x = positions(i) - positions(i - 1)
    } yield x
    val data = positions.head :: result.toList
    data.map(e => integer_mass_revers(e)).mkString("")
  }

}
