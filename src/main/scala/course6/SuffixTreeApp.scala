package course6

import scala.collection.mutable

object SuffixTreeApp extends App  {
  import SuffixTreeAdvance._

  val input =
    """
      |1 -> 2
      |2 -> 3
      |3 -> 4,5
      |6 -> 7
      |7 -> 6
      |""".stripMargin

  val tr = convertToTrie(input)
  val tree = maxNonBranching(tr)

  val d = tree
    .map(r => {
      val l = r.map(e => List(e.f, e.t)).flatten
      l
        .toList
        .mkString("->")
    })
  println(d.mkString("\n"))
  sys.exit(0)

  val in = "ATAAATG$"

  // "panamabananas$"
  // "ATAAATG$"
  //val res = construct(in)
  //res.merge()
  //val res2 = res.traverse(res, List.empty, List.empty)

  val res = modifiedSuffixTrieConstruction(in)
  res._1.foreach(path => {
    nonBranching(path, Some( (path, "") )) match {
      case Some((node, name)) if name.length > 1 =>
        val first = path.edge.head._1.pos
        path.edge.clear()
        val edge = Edge(name, first)
        path.edge = path.edge + (edge -> node)
        //println(name)
        //println(node)
      case _ =>

    }
  })
  val xs = allEdges(res._2, res._2.edge.keys.toList)

  val rs =
    xs
    .toSet
    .toList

  println( res )
  //println(res._2.toString)

}
