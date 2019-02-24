package com.binf

object Fun {
  private val alphabet = "ACGT"
  private val digitWeight: Map[Char, Int] = Map('A' -> 0, 'C' -> 1, 'G' -> 2, 'T' -> 3)

  def patternToNumber(pattern: String)(implicit k: Int): Int =
    pattern
      .reverse
      .zipWithIndex
      .map(p => digitWeight(p._1) * Math.pow(4, (p._2)).toInt)
      .sum

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
}
