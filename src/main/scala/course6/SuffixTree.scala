package course6


import scala.collection.immutable.List
import scala.collection.mutable

object SuffixTree {

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

  type Pair = (String, List[Int])
  def slideText(input: Pair, acc: List[Pair] = List.empty): List[Pair] =
  if (input._1.length >= 1) {
    slideText( (input._1.tail, input._2.tail), acc :+ input)
  } else {
    acc
  }

  def formattedPrint(implicit trie: Node): String = {
//        trie
//          .children
//          .map(c => s"${c._1}").mkString("") +
//          trie.children.map(kv => formattedPrint(kv._2)).mkString("\n")
    trie
      .children
      .map(c => s"\n${trie.index}->${c._2.index}:${c._1}").mkString("") +
      trie.children.map(kv => formattedPrint(kv._2)).mkString("")
  }

//  def getAllTree(text: String): List[String] = {
//    for {
//      tree <- slideText(text)
//      res = if (tree.length == 1)
//        List(tree)
//      else
//        getAllTree(tree.tail)
//    } yield res
//  }

  /*
  "AAATG$", "AAATG$", "G$", "T", "ATG$", "TG$", "A", "A", , "G$", "T", "G$", "$"
  "TAAATG$",  "ATAAATG$", "AATG$", "AAATG$", "G$", "ATG$", "TG$", "$")
   */
}
