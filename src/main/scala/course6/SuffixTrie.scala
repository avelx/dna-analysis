package course6

import course6.DnaMutation.formattedPrint

import scala.collection.immutable.List
import scala.collection.mutable

object SuffixTrie {

  var totalIndex = 1

  final case class Node(key: Char, index: Int, children: mutable.Map[Char, Node])

  private def add(current: Node, key: Char): Node =
    if (current.children.contains(key)) // no need to add node
      current
    else {
      val childNode = Node(key, totalIndex, mutable.Map[Char, Node]().empty)
      totalIndex += 1
      val childrenUpdated = current.children += (key -> childNode)
      val updated = current.copy(children = childrenUpdated)
      updated.children.get(key).get
    }

  private def addPattern(current: Node, pattern: String): Node = pattern.headOption match {
    case Some(key) if current.children.contains(key) =>
      addPattern(current.children.get(key).get, pattern.tail)
    case Some(key) =>
      val updated = add(current, key)
      addPattern(updated, pattern.tail)
    case None =>
      current
  }

  def createTrie(input: List[String]): Node = {
    val root = Node('R', 0, mutable.Map[Char, Node]())
    input.foreach(pattern => {
      addPattern(root, pattern)
    })
    root
  }

  def slideText(input: String, acc: List[String] = List.empty): List[String] = if (input.length > 1) {
    slideText(input.tail, acc :+ input)
  } else {
    acc
  }

  def formattedPrint(implicit trie: Node): String = {
    trie
      .children
      .map(c => s"\n${trie.index}->${c._2.index}:${c._1}").mkString("") +
      trie.children.map(kv => formattedPrint(kv._2)).mkString("")
  }

}
