package course4

object neighborJoining extends App {
  val D = Array(
//        Array(0, 13, 21, 22),
//        Array(13, 0, 12, 13),
//        Array(21, 12, 0, 13),
//        Array(22, 13, 13, 0)
    Array(0, 13, 16, 10),
    Array(13, 0, 21, 15),
    Array(16, 21, 0, 18),
    Array(10, 15, 18, 0)
  )
  val totalDistance = D.map(_.sum)
  val n = 4
  val D_ = for {
    i <- 0 to n - 1
    j <- 0 to n - 1
  } yield {
    if (i == j )
      0
    else
      (n - 2) * D(i)(j) - totalDistance(i) - totalDistance(j)
  }

 val F = D_.sliding(n, n).toArray

 F.foreach(row => println(row.mkString(" ")))

}
