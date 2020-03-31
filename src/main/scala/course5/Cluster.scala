package course5

object Cluster {

  case class Point(dimPos: Seq[Double])

  def d(v : Point, w: Point) : Double = {
    val result = for {
      i <- 0 to v.dimPos.length - 1
      delta = v.dimPos(i) - w.dimPos(i)
    } yield delta * delta
    Math.sqrt(result.sum)
  }

  /* distance from data point to neares centers */
  def dc(dataPoint: Point, centers: Seq[Point]) : Double = {
    val result = for {
      c <- centers
      distance = d(dataPoint, c)
    } yield  distance
    if (result.isEmpty) 0d else result.min
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

  def distortion(data : Seq[Point], centers: Seq[Point]) : Double = {
    val n = data.length
    val all = for {
      point <- data
      distance  = dc(point, centers)
    } yield distance * distance
    all.sum / n
  }

  def hiddenMatrix( i: Int, j: Int, data: Seq[Point], centers: Seq[Point]) : Double = {
    {
      val dx = d( data(j), centers(i) )
      1 / (dx * dx)
    } / {
      val all = for {
        t <- 0 to centers.length - 1
        xt = centers(t)
        dt = d( data(j), xt )
      } yield 1 / (dt * dt)
      all.sum
    }
  }

  def computeWeightedCenterGravity(i: Int, data: Seq[Point], hm: Array[Array[Double]]) : Point = {
    val allXY = for {
      j <- 0 to data.length - 1
      h = hm(i)(j)
      cx = data(j).dimPos(0) * h
      cy = data(j).dimPos(1) * h
    } yield (cx, cy)

    val hs = hm(i).sum

    Point( Seq( allXY.map(_._1).sum / hs , allXY.map(_._2).sum / hs  ) )
  }

  //TODO: https://stepik.org/lesson/10933/step/3?unit=9019
  def hiddenMatrixA( i: Int, j: Int, data: Seq[Point], x: Seq[Point], b : Double) : Double = {
    val e = 2.72d

    0d
  }

  def dmin(coord: Seq[Int], data: Array[Array[Int]]) : Int = {
    val res = for {
      x <- coord
      col = data(x).filter(_ > 0)
    } yield col.min
    res.min
  }

  def davg(coord: Seq[Int], data: Array[Array[Int]]) : Int = {
    val n = (data.head.length - 1) * 2
    val res = for {
      x <- coord
      col = data(x).filter(_ > 0)
    } yield col.sum
    res.sum / n * n
  }

}
