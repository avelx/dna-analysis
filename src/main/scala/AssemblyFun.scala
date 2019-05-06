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

  //def deBruijnGraphFromKmers(kmers: Seq[String]): String = {
  def deBruijnGraphFromKmers(kmers: Seq[String]): Array[Array[String]] = {

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

    //r.toList.sortBy(_._1).map(row => s"${row._1} -> ${row._2.mkString(",")}" ).mkString("\n")
    r.toArray.sortBy(_._1).map(row => row._1 +: row._2.toArray[String] )
    //.map(row => s"${row._1} -> ${row._2.mkString(",")}" ).mkString("\n")
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

  def eulerianCycle(g: Graph, size: Int): String = ???


  def eulerianPath(graph:  Graph, size: Int) : Array[Int] = {

    case class Node(i: Int) {
      def hasNeighbours: Boolean = neighbours.size > 0

      val neighbours = ListBuffer[Int]()

      def addNeigbour(i: Int) = neighbours.append(i)

      def remove(i: Int) = {
        val index = neighbours.indexOf(i)
        if (index == -1)
          throw new Error("Index not exists")
        else
          neighbours.remove(index)
      }
    }

    //val (graph, size) = toGraph(graphAsString_)

    val nodes = new ListBuffer[Node]()
    graph.map(n => {
      val node = new Node(n.head)
      n.tail.map(nb => node.addNeigbour(nb))
      nodes.append(node)
    })

    ///////////////////////////////////////////////////////
    // PrepProcess
    val inNodes = new Array[Int](size)
    val outNodes = new Array[Int](size)
    nodes.map(n => {
      outNodes(n.i) = outNodes(n.i) + n.neighbours.length
      n.neighbours.map(nb => inNodes(nb) = inNodes(nb) + 1)
    })

    val unbalancedNodes = (0 to size - 1).map(i => if (inNodes(i) != outNodes(i)) i else 0).filter(_ > 0)
    val unbalancedFrom = unbalancedNodes.map(i => if (inNodes(i) > outNodes(i)) i else 0 ).filter(_ > 0)
    val unbalancedTo = unbalancedNodes.map(i => if (inNodes(i) < outNodes(i)) i else 0 ).filter(_ > 0)

    val unbalancedPairs = unbalancedFrom zip unbalancedTo

    unbalancedPairs.foreach(p => {
      nodes.find(_.i == p._1) match {
        case None =>
          val node = Node(p._1)
          node.addNeigbour(p._2)
          nodes.append(node)
        case Some(node) =>
          node.addNeigbour(p._2)
      }
    })

    ///////////////////////////////////////////////////////

    val numberOfEdges = graph.map(_.tail.map(_ => 1)).flatten.sum
    val circuit: Array[Int] = Array.fill(numberOfEdges + 2 )(0)
    var circuitPos: Int = 0

    def findCircuit(node: Node): Boolean = {
      if (!node.hasNeighbours) {
        circuit(circuitPos) = node.i
        circuitPos += 1
        true
      } else {
        while (node.hasNeighbours) {
          val index = Random.nextInt(node.neighbours.size)
          val nodeId = node.neighbours(index)
          node.remove(nodeId)
          val new_node = nodes.find(n => n.i == nodeId).getOrElse(Node(-1))
          //          if (new_node.i == -1)
          //            println(new_node)
          findCircuit(new_node)
        }
        circuit(circuitPos) = node.i
        circuitPos += 1
        false
      }
    }

    val node = nodes.find(n => n.i == unbalancedPairs.headOption.map(_._2) ).getOrElse( nodes(0) )
    val res = findCircuit(node)

    circuit.reverse.init.toArray
  }

  def stringReconstruction(kmers : Seq[String]) : String = {
    val kmersIndexes = kmers
      .map(x => Seq(x.init, x.tail) )
      .flatten
      .zipWithIndex
      .toMap[String, Int]

    val kmersIndexesRev = kmersIndexes.toList.map(p => (p._2, p._1) ).toMap[Int, String]

    val g = deBruijnGraphFromKmers(kmers)
    val graph = g.map(y => y.map(x =>kmersIndexes(x) ) )
    val size = graph.flatten.max + 1

    val path = eulerianPath(graph, size)
    val path_ = path.map(i => kmersIndexesRev(i) )

    val text = pathToGenome( path_ )
    text
  }

}
