package course5

object Cluster {

  case class Point(dimPos: Seq[Double])

  def d(v : Point, w: Point) : Double = {
    val result = for {
      i <- 0 to v.dimPos.length - 1
      delta = v.dimPos(i) - w.dimPos(i)
    } yield delta * delta
    Math.sqrt(result.sum.toDouble)
  }

  /* Distance from DataPoint to Centers */
  def dc(dataPoint: Point, centers: Seq[Point]) : Double = {
    val result = for {
      c <- centers
      distance = d(dataPoint, c)
    } yield  distance
    result.min
  }

  def maxDistance(data: Seq[Point], centers: Seq[Point]) : (Double, Point) = {
    val result = for {
      point <- data
      distance = dc(point, centers)
    } yield (distance, point)
    result.maxBy(_._1)
  }

  def farthesFirstTraversal(k: Int, m: Int, points: Seq[Point]) : Seq[Point] = {
    var centers = Set[Point]()
    while (centers.size < k){
      val pair = maxDistance(points, centers.toList)
      centers = centers + pair._2
    }
    centers.toList
  }

}
