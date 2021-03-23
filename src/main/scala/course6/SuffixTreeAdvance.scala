package course6

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

}
