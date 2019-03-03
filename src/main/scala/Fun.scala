package com.binf


object Fun {
  private val alphabet = "ACGT"
  private val digitWeight: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)
  private val numberToSymbol: Map[Int, Char] = Map(0 -> 'A', 1 -> 'C', 2 -> 'G', 3 -> 'T')
  private val complement : Map[Char, Char] = Map('A' -> 'T', 'T' -> 'A', 'C' -> 'G', 'G' -> 'C')

  def patternToNumber(pattern: String): Int =
    pattern
      .reverse
      .zipWithIndex
      .map(p => digitWeight(p._1) * Math.pow(4, (p._2)).toInt)
      .sum

  def numberToPattern(index: Int)(implicit k: Int) : String = {
    def quotient(n: Int, m: Int ) : Int = n / m
    def remainder(n: Int, m: Int ): Int = n % m

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
    val freq = new Array[Int]( Math.pow(4, k).toInt )

    def computingFreqAcc(s: String): String = s.length match {
      case x if (x > k) => {
        val number = patternToNumber( s.take(k) )
        freq(number) = freq(number) + 1
        computingFreqAcc(s.drop(1))
      }
      case y if (y == k) => {
        val number = patternToNumber( s.take(k) )
        freq(number) = freq(number) + 1
        ""
      }
    }

    computingFreqAcc(text)

    freq
  }

  def reverseComplement(text: String) : String = text.map( complement(_) ).reverse.mkString("")

  def patternIndecs(patter: String, genome: String) = {
    def patternIndecsAcc(start: Int, acc: List[Int]) : List[Int] = genome.indexOf(patter, start) match {
      case -1 => acc
      case x => patternIndecsAcc(x + 1, x :: acc)
    }
    patternIndecsAcc(0, List() )
      .reverse
  }

  def clumpFinding(genome: String, k: Int, L: Int, t: Int) : Seq[String] = ???

  def skew(genome: String) : List[Int] = {
    def skewAcc(g: List[Char], acc: List[Int], state: Int) : List[Int] = g match {
      case h::tail => h match {
        case 'C' => skewAcc(tail, acc :+ (state - 1), state - 1 )
        case 'T' => skewAcc(tail, acc :+ state, state  )
        case 'A' => skewAcc(tail, acc :+ state, state)
        case 'G' => skewAcc(tail, acc :+ (state + 1), state + 1 )
      }
      case Nil => acc
    }
    skewAcc(genome.toList, List(0), 0)
  }

  def skewMin(genome: String): List[Int] = {
    val ss = skew(genome)
    val ssWithIndex = ss.zipWithIndex
    val minVal = ssWithIndex.minBy(_._1)._1
    ssWithIndex.foldLeft( List[Int]() )( (a, b) => if (b._1 == minVal) a :+ b._2 else a)
  }


}
