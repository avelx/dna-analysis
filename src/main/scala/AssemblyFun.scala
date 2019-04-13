package com.dna.assembly

import scala.collection.mutable.ListBuffer

object AssemblyFun {

  def composition(dna: String, k: Int): Seq[String] = {
    val result = new ListBuffer[String]()
    dna.foldLeft(Seq[Char]())((r, c) => {
      if (r.length == k) {
        result.append(r.mkString(""))
        r.tail :+ c
      } else {
        r :+ c
      }
    })
    (result :+ dna.takeRight(k)).sorted.toSeq
  }

  def pathToGenome(kmers: Seq[String]): String = {
    val n = kmers.head.length - 1
    def pathToGenomeAcc(s: String, mers: Seq[String]) : String = mers.find(k => k.startsWith(s.takeRight(n)) ) match {
      case Some(r) => {
        val i = mers.zipWithIndex.find(x => x._1 == r).get._2
        pathToGenomeAcc(s :+ r.last, mers.drop(i)) }
      case None => s
    }

    pathToGenomeAcc(kmers.head, kmers.tail)
  }


}
