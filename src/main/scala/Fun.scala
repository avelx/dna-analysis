package com.binf

import scala.collection.mutable.ListBuffer


object Fun {
  private val alphabet = "ACGT"
  private val digitWeight: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
  private val numberToSymbol: Map[Int, Char] = Map(0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')
  private val complement: Map[Char, Char] = Map('A' -> 'T', 'T' -> 'A', 'C' -> 'G', 'G' -> 'C')

  def patternToNumber(pattern: String): Int =
    pattern
      .reverse
      .zipWithIndex
      .map(p => digitWeight(p._1) * Math.pow(4, (p._2)).toInt)
      .sum

  def numberToPattern(index: Int)(implicit k: Int): String = {
    def quotient(n: Int, m: Int): Int = n / m

    def remainder(n: Int, m: Int): Int = n % m

    def numberToPatternAcc(i: Int, k_ : Int): String = k_ match {
      case 1 => numberToSymbol(i).toString
      case x => {
        val prefixIndex = quotient(i, 4)
        val r = remainder(i, 4)
        val s = numberToSymbol(r)
        val prefixPattern = numberToPatternAcc(prefixIndex, k_ - 1)
        prefixPattern + s.toString()
      }
    }

    numberToPatternAcc(index, k)
  }

  def computingFreq(text: String)(implicit k: Int): Array[Int] = {
    val freq = new Array[Int](Math.pow(4, k).toInt)

    def computingFreqAcc(s: String): String = s.length match {
      case x if (x > k) => {
        val number = patternToNumber(s.take(k))
        freq(number) = freq(number) + 1
        computingFreqAcc(s.drop(1))
      }
      case y if (y == k) => {
        val number = patternToNumber(s.take(k))
        freq(number) = freq(number) + 1
        ""
      }
    }

    computingFreqAcc(text)

    freq
  }

  def reverseComplement(text: String): String = text.map(complement(_)).reverse.mkString("")

  def patternIndecs(patter: String, genome: String) = {
    def patternIndecsAcc(start: Int, acc: List[Int]): List[Int] = genome.indexOf(patter, start) match {
      case -1 => acc
      case x => patternIndecsAcc(x + 1, x :: acc)
    }

    patternIndecsAcc(0, List())
      .reverse
  }

  def clumpFinding(genome: String, k: Int, L: Int, t: Int): Seq[String] = ???

  def skew(genome: String): List[Int] = {
    def skewAcc(g: List[Char], acc: List[Int], state: Int): List[Int] = g match {
      case h :: tail => h match {
        case 'C' => skewAcc(tail, acc :+ (state - 1), state - 1)
        case 'T' => skewAcc(tail, acc :+ state, state)
        case 'A' => skewAcc(tail, acc :+ state, state)
        case 'G' => skewAcc(tail, acc :+ (state + 1), state + 1)
      }
      case Nil => acc
    }

    skewAcc(genome.toList, List(0), 0)
  }

  def skewMin(genome: String): List[Int] = {
    val ss = skew(genome)
    val ssWithIndex = ss.zipWithIndex
    val minVal = ssWithIndex.minBy(_._1)._1
    ssWithIndex.foldLeft(List[Int]())((a, b) => if (b._1 == minVal) a :+ b._2 else a)
  }

  def hammingDistance(a: String, b: String): Int = a.zipWithIndex.foldLeft(0)((a, p) => if (p._1 != b(p._2)) a + 1 else a)

  def approximateOccurrences(genome: String, pattern: String, distance: Int): Seq[Int] =
    for {
      index <- (0 to genome.length - pattern.length)
      candidate = genome.drop(index).take(pattern.length)
      currentDistance = hammingDistance(pattern, candidate)
      if (currentDistance <= distance)
    } yield index


  def approximatePatternCount(genome: String, pattern: String, d: Int): Int = ???

    def neighbors(pattern: String, distance: Int): Seq[String] = {
      val alphabet = "ACGT"

      def neighborsAcc(acc: Seq[String]): Seq[String] =
        acc.map(p => {
          for {
            index <- (0 to p.length - 1)
            candidate <- alphabet.toList
            result = p.indices.map(i => if (i != index) p(i) else candidate)
          } yield result.mkString("")
        }).flatten

      var result = Seq(pattern)
      for (i <- 0 to distance - 1) {
        result = neighborsAcc(result)
      }
      result.distinct
    }

  /* possibly contains wrong logic in kmers generation */
  def freqWordsWithMismatches(genome: String, k: Int, d: Int, withReverse: Boolean = false): List[String] = {

    def getKMers(index: Int, acc: List[String]): List[String] = {
      val kmer = genome.drop(index).take(k)
      if (kmer.length == k)
        getKMers(index + 1, acc :+ kmer)
      else
        acc
    }

    val kmers = getKMers(0, List())


    def getFreq(kMer: String, freq: Int, in: List[String]): Int = in match {
      case h :: tail => if (hammingDistance(kMer, h) <= d) getFreq(kMer, freq + 1, tail) else getFreq(kMer, freq, tail)
      case Nil => freq
    }


    val freq = collection.mutable.Map[String, Int]()

    if (!withReverse) {

      kmers
        .distinct
        .foreach(kmer => {
          val f = getFreq(kmer, 0, kmers)
          freq(kmer) = f
        })

      val r = freq.maxBy(_._2)
      freq.toList.filter(_._2 == r._2).map(_._1)
    } else {


      kmers.distinct
        .foreach(kmer => {
          val f = getFreq(kmer, 0, kmers)
          val fr = getFreq(reverseComplement(kmer), 0, kmers)
          freq(kmer) = f + fr
        })

      val r = freq.maxBy(_._2)._2
      println(r)
      freq.toList.foreach(println)
      freq.toList.filter(_._2 == r).map(_._1)
    }
  }

  def patterns(s: String)(implicit k: Int ) : Seq[String] = {
    val result = new ListBuffer[String]()
    s.foldLeft(Seq[Char]())((r, c) => {
      if (r.length == k) {
        result.append(r.mkString(""))
        r.tail :+ c
      } else {
        r :+ c
      }
    })
    (result :+ s.takeRight(k)).distinct
  }

  def motifEnumeration(DNA: Seq[String], k: Int, d: Int): Seq[String] = {
    val ps = new ListBuffer[String]()
    val allPatterns = DNA.map(patterns(_)(k)).flatten.distinct
    allPatterns.foreach(pn => {
      val pnWithNB = neighbors(pn, d).filter(_ != pn)
      pnWithNB.foreach(p => {
        val res = DNA.forall(str => {
          val stR = patterns(str)(k)
          stR.find(x => hammingDistance(x,p) <= d ).isDefined
        })
        if (res)
          ps.append(p)
        //val rr = DNA.forall(s => patterns(s)(k).exists(dd => if (hammingDistance(dd, p) < d) true else false ))
        //if (rr)
        //  ps.append(p)
        //rr.flatten.map(dd => if (hammingDistance(dd, p) < d) ps.append(dd) )
      })
        //if ( DNA.exists(dna => patterns(dna)(k).exists(dd => if (hammingDistance(dna, p) < d) ) ps.append(dd)  )
    })
    ps.distinct
  }

  def distanceBetweenPatternAndString(pattern: String, dna: Seq[String]) : Int = {
    val k = pattern.length
    var distance : Int = 0
    dna.foreach(text => {
      var hd = Int.MaxValue
      val ps = patterns(text)(k)
      ps.foreach(p => {
        val h = hammingDistance(pattern, p)
        if (hd > h) hd = h
      } )
      distance += hd
    })
    distance
  }

  def medianString(dna: Seq[String], k: Int) : String = {
    var distance : Int = Int.MaxValue
    var median : String = ""
    (0 to Math.pow(4, k).toInt - 1)
      .toList.map(i => {
        val pattern = numberToPattern(i)(k)
        val d = distanceBetweenPatternAndString(pattern, dna)
        if ( distance > d) {
          distance = d
          median = pattern
        }
    })
    median
  }

}
