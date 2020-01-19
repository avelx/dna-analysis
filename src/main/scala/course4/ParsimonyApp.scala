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

  val leafStr = tree
      .map(_.split("->") )
      .filter(x => Try(x(1).toInt).toOption == None)

  val dna = leafStr.map(_(1)).toList
  val edges = tree
    .map(_.split("->") )
    .filter(x => Try(x(1).toInt).toOption != None)
    .map(x => List(x(0).toInt, x(1).toInt) )

  case class Leaf(id: Int, v: String, score: Array[Array[Int]])

  val leafs = leafStr.map(leaf =>
    Leaf(
      leaf(0).toInt,
      leaf(1),
      leaf(1).toArray.map(c => alphabet.map(x => if (x == c) 0 else 1) )
    )
  )

  var listNode = leafs.clone()
  val allLeafs = ListBuffer(leafs: _*)

  while( listNode.size > 1 ) {
    val mergeNodes = listNode.groupBy(_.id).map(p => {
      val (left, right) = (p._2(0), p._2(1))
      merge(left, right)
    })
    allLeafs.appendAll(mergeNodes)
    listNode = mergeNodes.toArray
  }

  def merge(l: Leaf, r: Leaf): Leaf = {
    val res = for {
      i <- 0 to l.v.length - 1
      s = l.score(i).zip(r.score(i)).map(p => p._1 + p._2)
    } yield s
    val v = res.map(x => alphabet(x.zipWithIndex.minBy(_._1)._2) ).mkString("")
    edges.find(e => e.tail.contains(l.id) ) match {
      case Some(ls) => Leaf(ls(0), v, res.toArray)
      case None => Leaf(-1, v, res.toArray)
    }
  }

  def getRootNodeId(nodeId : Int) : Int = edges.find(edge => edge(1) == nodeId)  match {
    case Some(ls) => getRootNodeId(ls(0))
    case None => nodeId
  }

  val rootNodeId = getRootNodeId(edges.head.last)

  def hammingDistance(x: String, y: String) = {
    var result = 0
    for {
      i <- 0 to x.length - 1
    } yield {
      if (x(i) != y(i)) result += 1
    }
    result
  }
  def iterateTree(parentId: Int) : Unit = edges.filter(e => e(0) == parentId) match {
    case children => if (children.length > 0){
      // Computation
      val from = allLeafs.find(n => n.id == parentId).get
      for {
        child <- children
        node = child(1)
        to <- allLeafs.filter(n => n.id == node)
        distance = hammingDistance(from.v, to.v)
      } yield {
        println(s"${from.v}->${to.v}:$distance")
      }
      children.map(child => iterateTree(child(1)))
    }
  }

  allLeafs.find(n => n.id == -1) match {
    case Some(root) => {
      println(root.score.map(_.min).sum)
      iterateTree(rootNodeId)
    }
    case None => throw new Error("Processing error")
  }

  /*
    Nodes tree represantation is incorrect
    id and parentId is wired incorrect
    Possibly need to introduce new type? => Leaf ?
   */
}