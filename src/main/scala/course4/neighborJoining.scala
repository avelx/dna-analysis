package course4

object c4week3Runner extends App {
  type Matrix = Array[Array[Int]]

  val d = Array(
    Array(0, 13, 21, 22),
    Array(13, 0, 12, 13),
    Array(21, 12, 0, 13),
    Array(22, 13, 13, 0)
  )
  val n = d.head.length
  val nodes = (0 to n - 1).toSet[Int]

  val totalDistance = d.map(_.sum)
  val d_ = for {
    i <- 0 to n - 1
    j <- 0 to n - 1
  } yield if (i != j) (n - 2) * d(i)(j) - totalDistance(i) - totalDistance(j) else 0

  val result = d_.sliding(4, 4).toArray

  result.foreach(row => println(row.mkString(" ")) )

  val ms = minPair(result.map(_.toArray), n)
  //println(ms)
  println(nodes -- ms)

  var mr = Map[(Int, Int), Int]()
  // TODO: convert merge set into list of pair of coordinates
  (nodes -- ms).sliding(2, 2).foreach(p => {
    val (x, y) = (p.head, p.last)
    mr = mr + ((x, y ) -> d(x)(y) )
  })

  for {
    x <- (nodes -- ms).toList
    row = ( ms.map(i => d(x)(i) ).sum  - d( ms.head )( ms.last ) ) / 2
  } yield {
     mr = mr + ((x, ms.last) -> row)
  }

  mr.foreach(r => println(r))



  def minPair(mx: Matrix, N: Int ) : Set[Int] = {
    val minElement : Int = mx.flatten.min
    println(minElement)
      for (i <- 0 to N - 1) {
        for (j <- 0 to N - 1) {
          if (mx(i)(j) == minElement)
            return Set(i, j)
        }
      }
    Set(0, 0)
  }

  println("This is not a test")
}

object TreeReconstruction {

  def neighborJoining() : Unit = {

  }

}
