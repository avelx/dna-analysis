package course4

import scala.collection.mutable.ListBuffer
import scala.util.Try

object ParsimonyApp extends App {
  import course4.EvolutionaryTree.parsymonyScore

  val alphabet = Array('A', 'C', 'G', 'T')

  case class Leafs(left: String, right: String)

  val tree = Array(
    "4->CAAATCCC",
    "4->ATTGCGAC",
    "5->CTGCGCTG",
    "5->ATGGACGA",
    "6->4",
    "6->5"
  )

  val leafs = tree
      .map(_.split("->") )
      .filter(x => Try(x(1).toInt).toOption == None)

  val dna = leafs.map(_(1)).toList
  val edges = tree
    .map(_.split("->") )
    .filter(x => Try(x(1).toInt).toOption != None)
    .map(x => List(x(0).toInt, x(1).toInt) )

//  edges.foreach(x => println(x.mkString(" ")) )

  case class Node(id: Int, v: String, score: Array[Array[Int]])

  val nodes = leafs.map(leaf =>
    Node(
      leaf(0).toInt,
      leaf(1),
      leaf(1).toArray.map(c => alphabet.map(x => if (x == c) 0 else 1) )
    )
  )

  var listNode = nodes.clone()
  val allNodes = new ListBuffer[Node]()
  nodes.foreach(n => allNodes.append(n))

  while( listNode.size > 1 ) {
    val mergeNodes = listNode.groupBy(_.id).map(p => {
      val (left, right) = (p._2(0), p._2(1))
      merge(left, right)
    })
    allNodes.appendAll(mergeNodes)
    listNode = mergeNodes.toArray
  }


  def merge(l: Node, r: Node): Node = {
    val n = l.v.length

    val res = for {
      i <- 0 to n - 1
      s = l.score(i).zip(r.score(i)).map(p => p._1 + p._2)
    } yield s
    val v = res.map(x => alphabet(x.zipWithIndex.minBy(_._1)._2) ).mkString("")
    val parentId = edges.find(e => e.tail.contains(l.id) ).getOrElse( List(-1) )(0)
    Node(parentId, v, res.toArray)
  }

  //val sc = allNodes.find(_.id == -1).get.score.map(_.min).sum
  //println(sc)
  println( allNodes.find(_.id == -1).get.v )

  val (score, minStr) = parsymonyScore(dna)(alphabet)
  println(minStr)

}