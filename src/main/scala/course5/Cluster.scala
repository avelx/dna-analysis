package course5

object Cluster {

  case class Point(dimPos: Seq[Int])

  def d(v : Point, w: Point) : Double = {
    val result = for {
      i <- 0 to v.dimPos.length - 1
      delta = v.dimPos(i) - w.dimPos(i)
    } yield delta * delta
    Math.sqrt(result.sum)
  }


}
