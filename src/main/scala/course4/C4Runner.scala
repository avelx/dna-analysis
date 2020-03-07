package course4

object C4Runner extends  App {
  val D = Array(
    Array(0, 11, 2, 16),
    Array(11, 0, 13, 15),
    Array(2, 13, 0, 9),
    Array(16, 15, 9, 0)
  )

  val totalDistance = D.map(_.sum)

  val nodes = (0 to D.length - 1)
  var tmpMx : Map[Int, Map[Int, Int]] = nodes
    .map(e => (e, nodes.map(p => (p, 0)).toMap)).toMap

  for {
    i <- nodes.toList
    j <- nodes.toList
  } yield {
    val v = if (i != j) (D.length - 2) * D(i)(j) - totalDistance(i) - totalDistance(j) else 0
    var me = tmpMx(i)
    me = me + (j -> v)
    tmpMx = tmpMx + (i -> me)
  }

  tmpMx.foreach(row => println( row._2.values.mkString(" ")) )

}
