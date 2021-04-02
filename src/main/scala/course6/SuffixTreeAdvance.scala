package course6

import course6.SuffixTreeApp.{input, tr}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object SuffixTreeAdvance {

  trait BaseNode {
    var edge: mutable.Map[Edge, Node]
  }

  final case class Edge(symb: String, pos: Int)

  final case class Node(
                         var edge: mutable.Map[Edge, Node],
                         var label: Option[Int] = None) extends BaseNode

  def modifiedSuffixTrieConstruction(in: String) : (List[Node], Node) = {

    val root : Node = Node( mutable.Map.empty )

    val trie = ListBuffer[Node]( root )
    (0 to in.length - 1).foreach(i =>{
      var currentNode : Node = root

      (i to in.length - 1).foreach(j => {
        val currentSymbol = in(j)
        currentNode.edge.find(e => e._1.symb == currentSymbol.toString) match {
          case Some(p) =>
            val newCurrent = p._2
            currentNode = newCurrent
            //( currentSymbol.toString)
          case None =>
            val node = Node( mutable.Map.empty)
            currentNode.edge = currentNode.edge + ( Edge(currentSymbol.toString, j) -> node)
            trie.append(node)
            currentNode = node
        }
      })

      if (currentNode.edge.size == 0){
        currentNode.label = Some(i)
      }

    })
    (trie.toList, root)
  }

  def nonBranching(node: Node, acc: Option[(Node, String)]): Option[(Node, String)] = {
    node.edge match {
      case p if p.keys.size > 1 => None
      case p if p.keys.size == 1 =>
        val res = acc.get._2 + p.keys.head.symb
        nonBranching(node.edge.head._2, Some( (node.edge.head._2, res) ) )
      case _ => acc
    }
  }

  def allEdges(node: Node, acc: List[Edge]) : List[Edge] = node.edge match {
    case e if e.keys.size > 0 =>
      e.map(p => allEdges(p._2, p._1 +: acc))
        .toList.flatten
    case _ =>
      acc
  }

  final case class Ed(f: Int, t: Int)

  def convertToTrie(input : String) : List[Ed] = {
    input
      .split("\n")
      .map(_.replace(" ", ""))
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(row => {
        val arr = row.split("->")
        val from = arr(0).toInt
        arr(1)
          .split(",")
          .map(_.toInt)
          .map(to => Ed(from,to))
      })
      .toList
      .flatten
  }

  def maxNonBranching(trie: List[Ed]): List[List[Ed]] = {
    var localTrie : Map[String, Ed] = trie.map(e => (s"${e.f}->${e.t}" -> e) ).toMap

    def drillDownPath(start: (String, Ed), acc: List[Ed]) : List[Ed] = {
      val filtered = localTrie
        .filter(_._2.f == start._2.t)
//        .filter(p => acc.find(e => e == p._2).isEmpty)
      filtered.toList.length match {
        case 1 =>
          val e = filtered.values.head
          localTrie = localTrie - s"${e.f}->${e.t}"
          drillDownPath(filtered.head, filtered.head._2 +: acc)
        case _ =>
          acc
      }
    }

    trie.map( p => {
      drillDownPath((s"${p.f}->${p.t}", p), List(p))
    })

  }
}

object SuffixTree3 {

  class Trie(){
    var index = 0
    def nextIndex : Int = {
      index += 1
      index
    }
    class Node(var hasValue: Boolean,
               val index: Int,
               val children: mutable.Map[Char, Node] = mutable.Map[Char, Node]().empty )

    val root = new Node( false, index)


    def add(s: String) = {
      var current = root
      for(c <- s)
        current = current.children.getOrElseUpdate(c, new Node(false, nextIndex ) )
      current.hasValue = true
    }

    def contains(s: String) : Boolean = {
      var current = Option(root)
      for(c <- s if current.nonEmpty) current = current.get.children.get(c)
      current.exists(_.hasValue)
    }

    def travers(): List[(String, Int)] = {
      def accTravers(c: Node, path: (String, Int)) : List[(String, Int)] = {
        c.children.size match {
          case 1 =>
            accTravers(c.children.head._2, (c.children.head._1 +: path._1, path._2))
          case 0 =>
            List(path)
          case _ =>
              c
              .children
              .map(p => List(path) ++ accTravers(p._2, (p._1.toString, p._2.index) ) )
              .toList
              .flatten
        }
      }

      root
        .children
        .map(kv => accTravers(kv._2, (kv._1.toString, kv._2.index) ) )
        .toList
        .flatten
    }

    def edges() : List[String] = {
      val res = travers()
        .toSet
      res.toList
        .map(x => x._1)
        .map(_.reverse)
    }

  }
}