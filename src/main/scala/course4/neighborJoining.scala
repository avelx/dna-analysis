package course4

import scala.collection.mutable.ListBuffer

object c4week3Runner extends App {

  import TreeReconstruction._

  type Matrix = Map[Int, Map[Int, Int] ]
  type Tree = Map[Int, List[Int]]

  var tree : Tree = Map[Int, List[Int] ]()

  val data = ListBuffer[ Matrix ](  readMaxtrixFromFile("src/main/resources/data/dataset_nja.txt") )
  var distances = Map[(Int, Int), Int]()

  var originalSize = data.head.size
  var current : Matrix = data.head
  while(current.size > 2){
    current = iterate(current)
    data.append(current)
  }
  //current.foreach(row => println(row._2.values.mkString(" ")))

  var finalKeys : List[Int] = current.keys.toList
  val df : List[Int] = finalKeys.last :: tree(finalKeys.head)
  tree += (finalKeys.head -> df )

  val g = current(finalKeys.last)
  distances += ((finalKeys.head, finalKeys.last) -> g(finalKeys.head) )
  distances += ((finalKeys.last, finalKeys.head) -> g(finalKeys.head) )

  var delta : Int = g(finalKeys.head)

  //current = data.init.last


  distanceRecalc(data.toList.sortBy(_.size).tail)

  // TODO : Travers from joiner to then end of the tree with no calculated edges
  def distanceRecalc(in : List[ Matrix ]) : Unit = in match {
    case h::tail => {
      (finalKeys.toSet intersect h.keys.toSet[Int] ).foreach(joiner => {
                for {
                  x <- h(joiner).keys
                } yield {
                  val z = h(joiner)
                  if (joiner != x)
                    distances += ((joiner, x) -> (z(x) - delta))
                }
      })
      finalKeys = h.keys.toList
      //println(h.mkString(" "))
      distanceRecalc(tail)
    }
    case Nil => Unit
  }

  distances.foreach(row => println(s"${row._1} -> ${row._2}"))
  //tree.foreach( r => println(s"${r._1} -> ${r._2.mkString(" ")}" ) )

  def iterate(in: Map[Int, Map[Int, Int]]): Map[Int, Map[Int, Int]] = {
    val mergeSet = getPairs(in)

    val n = in.size
    var (matrix_, nodesUp) = prepareMatrix(n, mergeSet, in)

    val z = matrix_.keys.last
    for {
      x <- nodesUp.toList
      a = mergeSet.map(i => in(x)(i)).sum
      b = in(mergeSet.head)(mergeSet.last)
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

    tree += (originalSize -> List(merge.head, merge.last) )

    val nodesUp_ = nodesUp + originalSize
    originalSize += 1

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
