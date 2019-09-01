package com.dna.compare.genes

object Bio3 {

  def LCS(a: String, b: String) : Option[String] = {
    if (a.isEmpty || b.isEmpty)
      None
    else {
      val res = {
        for {
          size <- 1 to a.length
          pos <- 0 to a.length
          x = a.slice(pos, size)
          y = b.slice(pos, size)
          if x == y
        } yield x
      }
      res.filter(_.nonEmpty).sortBy(_.length).lastOption
    }
  }

}