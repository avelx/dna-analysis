package course6

import scala.collection.immutable._
import scala.collection.mutable

final case class Node(key: Char, index: Int, children : mutable.Map[Char, Node])

object DnaMutation {

  var totalIndex = 1

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
      val childNode = Node(key, totalIndex, mutable.Map[Char, Node]().empty)
      totalIndex += 1
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

  def formattedPrint(implicit trie: Node) : String = {
    trie
        .children
        .map(c => s"\n${trie.index}->${c._2.index}:${c._1}").mkString("") +
      trie.children.map( kv => formattedPrint(kv._2)).mkString("")
  }

  /* Returns: matching text prefix with any patterns or None if no match found */
  def prefixTrieMatching(textPair: (String, Int), trie: Node): Option[(String, Int)] = {

    def prefixTrieMatchingAcc(key: Char, current: Node, input: String, res: List[Char]) : Option[String] = {
      if (!current.children.isEmpty){
        if (current.children.contains(key)){
          if (input.length >= 1)
            prefixTrieMatchingAcc(input.head, current.children.get(key).get, if (input.length > 1) input.tail else "", res :+ key)
          else
            if (res.length > 0 && current.children.get(key).get.children.isEmpty )
              Some(res.mkString(""))
            else
              None
        } else
          None
      } else {
        if (res.length > 0) Some(res.mkString("")) else None
      }
    }
//      if (!current.children.isEmpty && current.children.contains(key) && input.length > 1)
//        prefixTrieMatchingAcc(input.head, current.children.get(key).get, input.tail, res :+ key)
//      else if (!current.children.isEmpty)
//        None
//      else
//        Some(res.mkString(""))

    prefixTrieMatchingAcc(textPair._1.head, trie, textPair._1.tail, List.empty)
      .map( (_, textPair._2) )
  }

  def slideText(input: (String, Int), acc: List[(String, Int)]) : List[(String, Int)] = if (input._1.length > 1){
    slideText((input._1.tail, input._2 + 1), acc :+ input)
  } else {
    acc
  }

  def countLeaves(current: Node) : Int = {
    current.children.foldLeft(0)( (c, p) =>
      if (p._2.children.size > 0) c + p._2.children.map(e => countLeaves(e._2) ).sum
      else 1
    )
  }

}
