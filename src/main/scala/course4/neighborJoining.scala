package course4

import spire.math.algebraic.BigDecimalApproximations.MathContextOps

object c4week3Runner extends App {

  import TreeReconstruction._

  type Matrix = Array[Array[Int]]

  val ddd = readMaxtrixFromFile("src/main/resources/data/dataset_nja.txt")
  var size_ = ddd.size
  val dA = iterate(ddd)
  val dB = iterate(dA)

  dB.foreach(row => println(row._2.values.mkString(" ")))

  //  val ms = getPairs(d)
  //  val n = d.size
  //  var (matrix_, nodesUp) = prepareMatrix(n, ms, d)
  //
  //  println(nodesUp)
  //  for {
  //    x <- nodesUp.toList
  //    row = ( ms.map(i => d(x)(i) ).sum  - d( ms.head )( ms.last ) ) / 2
  //  } yield {
  //    var me = matrix_(x)
  //    me = me + (n -> row)
  //    matrix_ = matrix_ + (x -> me)
  //
  //    var me_ = matrix_(n)
  //    me_ = me_ + (x -> row)
  //    matrix_ = matrix_ + (n -> me_)
  //     //mr = mr + ((x, ms.last) -> row)
  //  }
  //  matrix_.foreach(row => println(row._2.values.mkString(" ")))

  def iterate(in: Map[Int, Map[Int, Int]]): Map[Int, Map[Int, Int]] = {
    val ms = getPairs(in)
    val n = in.size
    var (matrix_, nodesUp) = prepareMatrix(n, ms, in)

    val z = matrix_.keys.last
    for {
      x <- nodesUp.toList
      a = ms.map(i => in(x)(i)).sum
      b = in(ms.head)(ms.last)
      row =  (a - b) / 2
    } yield {
      var me = matrix_(x)
      me = me + (z -> row)
      matrix_ = matrix_ + (x -> me)

      var me_ = matrix_(z)
      me_ = me_ + (x -> row)
      matrix_ = matrix_ + (z -> me_)
      //mr = mr + ((x, ms.last) -> row)
    }

    matrix_
  }

  def prepareMatrix(k: Int, merge: Set[Int], f: Map[Int, Map[Int, Int]]): (Map[Int, Map[Int, Int]], Set[Int]) = {
    val nodes = f.keys.toSet[Int]
    val nodesUp = nodes -- merge
    val nodesUp_ = nodesUp + size_
    size_ += 1

    var tmpMx_ : Map[Int, Map[Int, Int]] = nodesUp_
      .map(e => (e, nodesUp_.map(p => (p, 0)).toMap)).toMap

    nodesUp.sliding(2, 2).foreach(p => {
      val (x, y) = (p.head, p.last)
      var me = tmpMx_(x)
      me = me + (y -> f(x)(y))
      tmpMx_ = tmpMx_ + (x -> me)
      var me_ = tmpMx_(y)
      me_ = me_ + (x -> f(x)(y))
      tmpMx_ = tmpMx_ + (y -> me_)
    })

    (tmpMx_, nodesUp)
  }

  def getPairs(mx: Map[Int, Map[Int, Int]]): Set[Int] = {
    val totalDistance : Map[Int, Int] = mx.map(p => (p._1, p._2.values.sum) )
    val nodes = mx.keys.toSet
    var tmpMx : Map[Int, Map[Int, Int]] = nodes
      .map(e => (e, nodes.map(p => (p, 0)).toMap)).toMap

    for {
      i <- mx.keys.toList
      j <- mx.keys.toList
    } yield {
      val v = if (i != j) (mx.size - 2) * mx(i)(j) - totalDistance(i) - totalDistance(j) else 0
      var me = tmpMx(i)
      me = me + (j -> v)
      tmpMx = tmpMx + (i -> me)
    }

    //val result = tmp.sliding(mx.size, mx.size).toArray
    val m = minPair(tmpMx)
    m
  }

  def minPair(mx: Map[Int, Map[Int, Int]]): Set[Int] = {
    val minElement: Int = mx.values.map(_.values).flatten.min
    for (i <- mx.keys) {
      for (j <- mx.keys) {
        if (mx(i)(j) == minElement)
          return Set(i, j)
      }
    }
    Set(0, 0)
  }

  println("This is not a test")
}

object TreeReconstruction {

  def readMaxtrixFromFile(filePath: String): Map[Int, Map[Int, Int]] = {
    val data = scala.io.Source.fromFile(filePath).getLines().toList
    val matrix = {
      for {
        r <- 0 to data.length - 1
        line = data(r).split(" ").zipWithIndex.map(p => (p._2, p._1.toInt))
      } yield (r, line.toMap)
      }.toMap
    matrix
  }

  def neighborJoining(): Unit = {

  }

}
