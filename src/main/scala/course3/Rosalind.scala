package com.funs

object Rosalind {

  def occurances(dna: String): Array[Int] =
    dna.foldLeft( Map[Char, Int]('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0) )( (acc, c) => acc + (c -> (acc(c) + 1)) ).map(_._2).toArray

}
