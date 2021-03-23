package course6

object SuffixTreeApp extends App  {
  import SuffixTreeAdvance._
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
