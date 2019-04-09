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


}
