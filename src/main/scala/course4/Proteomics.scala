package course4

import scala.collection.mutable.ListBuffer

object ProteomicsRunner extends App {

  import Proteomics._

 val vectorSpectrum = 0 :: "-3 -2 7 10 7 6 1 12 -7 -5 8 -6 -4 -2 1 -8 -4 11 -8 4 -10 -6 7 -8 10 -8 0 1 11 6 -10 5 3 2 -6 -7 -9 12 -9 7 12 15 -1 -8 0 10 -3 3 8 14 10 13 13 2 2 15 -5 5 0 11 5 2 7 8 15 9 5 12 15 15 0 10 6 1 5 9 -7 6 3 -3 0 -6 -4 -7 15 -7 13 -4 -1 15 -9 11 12 -8 8 -2 -9 -6 3 12 -2 14 -3 -6 10 1 -6 9 -10 0 7 15 9 12 4 -7 -10 -4 15 1 9 5 -6 -4 14 -1 11 -7 10 8 8 8 2 -2 -2 1 15 5 3 15 -5 13 5 -10 -4 -10 9 -6 -10 1 -6 -4 8 15 14 9 3 10 13 6 -10 -7 -4 6 11 5 13 7 -1 2 -1 9 2 8 15 10 -4 3 3 12 -3 14 4 -2 -8 8 -6 7 -2 12 -7 3 -9 15 0 -8 7 6 4 -2 -2 -8 -6 11 3 5 -7 2 -5 4 7 12 1 6 -2 13 9 11 14 -2 1 -4 7 1 -3 15 -5 5 -5 -2 15 -10 -7 3 0 -9 -8 -1 3 -6 -3 4 12 -5 -10 -9 14 9 -6 13 7 -1 13 -4 13 -3 8 12 -5 7 9 7 8 -1 8 14 8 -9 -5 -9 12 14 -4 11 -10 8 11 -10 -10 12 8 13 1 -9 13 7 3 12 -2 -1 1 4 -1 1 13 -8 14 3 -2 -3 13 -2 9 12 1 2 3 -6 6 -3 -7 6 -5 12 2 -1 9 -5 7 15 -3 -7 5 3 4 7 1 13 -8 8 3 12 15 -6 -8 15 4 0 -5 -5 -9 1 8 7 -7 -9 -7 2 -7 14 5 2 -7 11 3 1 -2 1 -8 8 -7 -9 4 -7 -9 -3 1 10 -6 -3 12 13 2 6 7 14 3 4 -9 5 -9 -9 5 -10 14 9 5 -6 4 -2 -3 4 -1 -1 1 -9 -3 0 -7 -5 -6 -5 12 -9 1 -1 14 5 0 -1 -4 4 3 4 3 2 15 6 15 -6 15 -5 12 15 15 3 0 2 0 -10 -8 12 15 -9 -4 3 -5 -4 -4 0 7 0 -4 -4 2 11 12 11 1 10 2 13 14 10 3 -1 1 11 2 8 -1 13 -4 6 -5 1 12 7 6 10 -6"
    .split(" ").map(_.toInt).toList

//    val vectorSpectrum = 0 :: "4 -3 -2 3 3 -4 5 -3 -1 -1 3 4 1 3"
//      .split(" ").map(_.toInt).toList

  val threshold = 38
  val maxScore = 200

  println( getSpectralDictionarySize(vectorSpectrum, threshold, maxScore, integer_mass_table_revers) )

  //  val spectralVectors = List(
  //    "-1 5 -4 5 3 -1 -4 5 -1 0 0 4 -1 0 1 4 4 4"
  //      .split(" ")
  //      .map(_.toInt).toList,
  //    "-4 2 -2 -4 4 -5 -1 4 -1 2 5 -3 -1 3 2 -3"
  //      .split(" ")
  //      .map(_.toInt).toList
  //  )
  //  val proteome = "XXXZXZXXZXZXXXZXXZX"
  //  val integer_mass : Map[String, Int] = Map("X" -> 4, "Z" -> 5)
  //  val threshold = 5
  //
  //  println(
  //    pmsSearch( spectralVectors, proteome, threshold)(integer_mass)
  //  )

  /*
      val data = scala.io.Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_11866_7.txt").getLines().toList

      val spectralVectors = data.init.init.map(s => s.split(" ").map(_.toInt).toList )

      val proteome = data.init.last
      val threshold = data.last.toInt

      println(
        pmsSearch( spectralVectors, proteome, threshold)(integer_mass_table_revers).mkString("\n")
      )
   */

}

object Proteomics {

  import scala.util.control.Breaks._

  def getSpectralDictionarySize(vectorSpectrum: List[Int], threshold: Int, maxScore : Int, integer_mass: Map[String, Int]): Int = {
    var size : Map[(Int, Int), Int] = Map( (0, 0) -> 1)

    def getSize(i: Int, t: Int): Int = {
      if ( size.contains( (i, t)) )
        size( (i, t) )
      else if ( i < 0 || t < 0) {
        size = size + ((i, t) -> 0)
        0
      } else {
        val s = integer_mass.map(_._2)
          .map(m => getSize( i - m, t - vectorSpectrum(i) )).sum
        size = size + ((i, t) -> s)
        s
      }
    }

    val i = vectorSpectrum.length - 1
    val result = {
      for {
        t <- threshold to maxScore + 1
      } yield getSize(i, t)
      }.sum
    result
  }

  def peptideIdentification(spectralVector: List[Int], proteome: String)(integer_mass: Map[String, Int]): (String, Int) = {
    val n = proteome.length
    val l = spectralVector.length
    val results = ListBuffer[(String, Int)]()
    for (i <- 0 to n - 1) {
      breakable {
        for (j <- i to n - 1) {
          val candidate = proteome.substring(i, j + 1)
          val candidateScore = candidate.map(c => integer_mass(c.toString)).sum
          if (candidateScore > l) break()
          else if (candidateScore == l) {
            val prefixesMasses = (getPrefixes(candidate) :+ candidate)
              .map(r => r.map(c => integer_mass(c.toString)).sum)
            val currScore = prefixesMasses.map(m => spectralVector(m - 1)).sum
            results.append((candidate, currScore))
          }
        }
      }
    }
    results.maxBy(_._2)
  }

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

  def getSuffixes(peptide: String): List[String] = {
    for {
      i <- 0 to peptide.length - 2
      p = peptide.substring(peptide.length - 1 - i, peptide.length)
    } yield p
    }.toList

  def peptideToSpectrum(peptide: String): List[Int] = {
    val prefixAndSuffix = getPrefixes(peptide) ++ getSuffixes(peptide)
    (prefixAndSuffix :+ peptide).map(row => row.map(c => integer_mass_table_revers(c.toString)).sum).sorted
  }

  def decodingIdealSpectrum(spectrum: List[Int]): String = {
    val graph = graphSpectrum(spectrum)

    def getPaths(s: Int, acc: List[Int]): List[List[Int]] = graph.filter(e => e._1 == s) match {
      case h :: tail => getPaths(h._2, h._1 :: acc) ++ tail.map(q => getPaths(q._2, q._1 :: acc)).flatten
      case Nil => List(s :: acc)
    }

    val result = getPaths(spectrum.head, List(0)).map(r => r.reverse)
    val peptides = result.map(row => {
      (0 to row.length - 2).map(i => integer_mass_table(row(i + 1) - row(i)))
    }).map(_.mkString(""))

    //peptides.foreach(r => println(r.mkString("") ) )
    peptides.find(peptide => peptideToSpectrum(peptide) == spectrum) match {
      case Some(p) => {
        println(p)
        p
      }
      case None => throw new Error("No peptide found for ideal spectrum")
    }

  }

  def getPeptideVector(peptide: String)(integer_mass: Map[String, Int]): String = {
    val prefixes = getPrefixes(peptide) :+ peptide

    val positions = prefixes.map(r => {
      r.map(c => integer_mass(c.toString)).sum
    })

    val z = Array.fill(positions.max)(0)
    positions.map(i => z(i - 1) = 1)
    z.mkString(" ")
  }

  def peptideVectorToPeptide(vector: String)(integer_mass_revers: Map[Int, String]): String = {
    val positions = vector.split(" ").zipWithIndex.map(p => if (p._1.equals("1")) p._2 + 1 else 0).filter(e => e > 0)
    val result = for {
      i <- 1 to positions.length - 1
      x = positions(i) - positions(i - 1)
    } yield x
    val data = positions.head :: result.toList
    data.map(e => integer_mass_revers(e)).mkString("")
  }

  def pmsSearch(spectralVectors: List[List[Int]], proteome: String, threashHold: Int)(integer_mass: Map[String, Int]): Set[String] = {
    var psm = Set[String]()

    spectralVectors.foreach(vector => {
      val peptidePair = peptideIdentification(vector, proteome)(integer_mass)
      if (peptidePair._2 >= threashHold)
        psm = psm + peptidePair._1
    })

    psm
  }

}
