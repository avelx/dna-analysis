package course6

import scala.collection.immutable._
import scala.collection.mutable

final case class Node(key: Char, index: Int, children : mutable.Map[Char, Node])

object DnaMutation {

  private def addPattern(current: Node, pattern: String): Node = pattern.headOption match {
    case Some(key) if current.children.contains(key) =>
      addPattern(current.children.get(key).get, pattern.tail)
    case Some(key) =>
      val updated = add(current, key)
      addPattern(updated, pattern.tail)
    case None =>
      current
  }

  private def add(current: Node, key: Char) : Node =
    if (current.children.contains(key)) // no need to add node
      current
    else {
      val childNode = Node(key, current.index + 1, mutable.Map[Char, Node]().empty)
      val childrenUpdated = current.children += (key -> childNode)
      val updated = current.copy( children = childrenUpdated )
      updated.children.get(key).get
    }

  def createTrie(input: List[String]) : Node = {
    val root = Node('R', 0, mutable.Map[Char, Node]())
    input.foreach(pattern => {
      addPattern(root, pattern)
    })
    root
  }

  def formattedPrint(current: Node) : String = {
    s"${current.key}:${current.index}" + " -> " + current.children.map( kv => formattedPrint(kv._2)).mkString("\n")
  }

}