package com.dna.assembly

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object AssemblyFun {

  type Graph = Array[Array[Int]]

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

    def pathToGenomeAcc(s: String, mers: Seq[String]): String = mers.find(k => k.startsWith(s.takeRight(n))) match {
      case Some(r) => {
        val i = mers.zipWithIndex.find(x => x._1 == r).get._2
        pathToGenomeAcc(s :+ r.last, mers.drop(i))
      }
      case None => s
    }

    pathToGenomeAcc(kmers.head, kmers.tail)
  }

  def overlapGraph(kmers: Seq[String]): String = {
    val res =kmers.map(k => {
      val bf = new ListBuffer[String]()
      bf.append(k)
      kmers.foreach(c => {
        if (c != k && c.startsWith(k.tail))
          bf.append(c)
      })
      bf.toList
    }).filter(l => l.length > 1)
    res.map(l => {
      val s = s"${l.head} -> ${l.tail.mkString(",")}"
      s
    }).mkString("\n")
  }

  def deBruijnGraphFromKmers(kmers: Seq[String]): String = {

    val sp = kmers.map(s => Seq(s.init, s.tail) ).flatten
    var kv = mutable.Map[String, Int]()
    var i = 0
    sp.map(x => {
      if (!kv.keys.exists(v => v.equals(x) )) {
        kv = kv + (x -> i)
        i += 1
      }
    })

    val kvInv = kv.toList.map(p => (p._2, p._1)).toMap

    val size = kv.keys.toList.length
    val matrix = Array.ofDim[Int]( size, size)
    kmers
      .map(s => (s.init, s.tail) )
      .map(p => {
        val x = kv(p._1)
        val y = kv(p._2)
        matrix(x)(y) = matrix(x)(y) + 1
      })

    //matrix.map(row => println(row.mkString(" ")) )

    val res = for{
      x <- 0 to size - 1
      y <- 0 to size - 1
      key = kvInv(x)
      if (matrix(x)(y) > 0)
      vals = Seq.fill(matrix(x)(y))(kvInv(y))
    } yield (key, vals)

    var r = mutable.Map[String, Seq[String]]()

    res.map(p => {
      if ( r.keys.exists(x => x.equals(p._1) ))
        r = r + (p._1 -> (p._2 ++ r(p._1)) )
      else
        r = r + (p._1 -> p._2)
    })

    r.toList.sortBy(_._1).map(row => s"${row._1} -> ${row._2.mkString(",")}" ).mkString("\n")
  }

  def deBruijinGrapFromString(dna: String, k: Int) : String = {
    val acc = new ListBuffer[String]()
    dna.foldLeft("")( (a, c) => if (a.length == k) {
      acc.append(a)
      a.tail :+ c } else a :+ c)

    val kmers = acc.toList
    val kmap_ = kmers.zip((0 to kmers.length - 1)).toMap
    val m = kmers.zip(kmers.tail :+ kmers.head)

    val matrix = Array.ofDim[Int]( kmers.length, kmers.length)

    m.map(edge => {
      val x = kmap_(edge._1)
      val y = kmap_(edge._2)
      matrix(x)(y) = matrix(x)(y) + 1
      //matrix(y)(x) = matrix(y)(x) + 1
    })

    //println( matrix.map(_.mkString(" ")).mkString("\n") )
    val kmersArr = kmers.toArray

    val t = matrix.zipWithIndex.map(zi => {
      val key = kmersArr(zi._2)
      val res = zi._1.zipWithIndex.filter(_._1 > 0).map(p => if (p._1 > 0) kmersArr(p._2) else "").filter(!_.isEmpty).mkString(",")
      if (res.isEmpty) "" else s"$key -> $res"
    }).filter(!_.isEmpty)

    t.mkString("\n")
  }

  def eulerianCycle(g: Graph, size: Int): String = {

    val gf: Graph = Array.ofDim[Int](size, size)
    g.map(row => {
      val x = row.head
      row.tail.map(y => gf(x)(y) = gf(x)(y) + 1)
    })

    def dive(fk: Int, tk: Int, gxk: Graph, acc: List[Int]): Array[Array[Int]] = gxk(fk)(tk) match {
      case v: Int if ((v) > 0) => {
        gxk(fk)(tk) = gxk(fk)(tk) - 1
        gxk(tk).zipWithIndex.filter(p => p._1 > 0).map(_._2) match {
          case arr: Array[Int] if (arr.length > 0) =>
            arr.map(y => dive(tk, y , gxk, tk +: acc)).flatten
          case _ =>
            Array( (tk+: acc).toArray[Int] )
        }
      }
      case _ => Array((tk +: acc).toArray[Int])
    }

    def cycleAcc(from: Int, gx: Graph): Array[Array[Int]] = {
      val r = for {
        to <- gx(from).zipWithIndex.filter(_._1 > 0).map(_._2)
        ff = dive(from, to, gx, List(from) )
      } yield ff
      r.flatten
    }

    val edgesNumber = g.map(_.tail.map(_ => 1)).flatten.sum

    var pathsResult = Array[Array[Int]]()
    val froms = new ListBuffer[Int]()
    while ( !pathsResult.exists(res => res.length == edgesNumber - 1) && froms.length < size ){
      val from = Random.nextInt(size)
      if (!froms.contains(from)) {
        val paths = cycleAcc(from, gf.map(_.clone()))
        pathsResult = paths.filter(p => p.head == from)
        froms.append(from)
      }
    }

    pathsResult.headOption.map(_.reverse.mkString("->") ).getOrElse("")
  }

}
