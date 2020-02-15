package course4

import scala.collection.mutable.ListBuffer

object c4week3Runner extends App {

  import TreeReconstruction._

  type Matrix = Map[Int, Map[Int, Int] ]

  case class Edge(f: Int, t: Int)
  var tree  = ListBuffer[Edge]()

  val path = "src/main/resources/data/dataset_njc.txt"
  val data = ListBuffer[ Matrix ](  readMaxtrixFromFile(path) )
  var distances = Map[(Int, Int), Int]()

  var vertexCounter = data.head.size
  var current : Matrix = data.head
  while(current.size > 2){
    current = iterate(current)
    data.append(current)
  }
  tree.append( Edge(vertexCounter - 2, vertexCounter - 1) )
  tree.appendAll( tree.map(e => Edge(e.t, e.f)) )

  var finalKeys : List[Int] = current.keys.toList
  val g = current(finalKeys.last)
  distances += ((finalKeys.head, finalKeys.last) -> g(finalKeys.head) )
  distances += ((finalKeys.last, finalKeys.head) -> g(finalKeys.head) )

  var delta : Int = g(finalKeys.head)

  val base : List[Int] = List(vertexCounter - 2, vertexCounter - 1)

  val a : List[List[Int]] = travers(base.head, List(base.head) )

  // Filter or by size or by source (base?)
  val c = a.filter(e => e.length > 2).map(_.head)
  val b =  travers(c.head, List(c.head) ).sortWith((f,g) => f.length > g.length).map(_.reverse)

  distanceUpdate(a)
  distanceUpdate(b)

  distances.foreach(p =>
    println(s"${p._1._1}->${p._1._2}:" + f"${p._2}%2.3f"))

  def distanceUpdate(in : List[List[Int]]) : Unit = {
    in.foreach(path => {
      val res = {
        val x = findDistance( path.head, path.last)
        if (!x.isDefined )
          findDistance( path.last, path.head)
        else x
      }
      if ( res.isDefined){
        var d = res.get
        val coords = path.sliding(2).toList.reverse
        if ( distances.contains( (coords.head.head, coords.head.last) ) ){
          val z = distances( (coords.head.head, coords.head.last) )
          d = d - z
          coords.tail.foreach(c => {
            if ( !distances.contains( (c.head, c.last) ) ) {
              distances += ((c.head, c.last) -> d)
              distances += ((c.last, c.head) -> d)
            } else {
              d = d  - distances((c.head, c.last))
            }
          })
        }
        else if (distances.contains( (coords.last.head, coords.last.last) )) {
          val z = distances( (coords.last.head, coords.last.last) )
          d = d - z
          coords.reverse.tail.foreach(c => {
            if ( !distances.contains( (c.head, c.last) ) ) {
              distances += ((c.head, c.last) -> d)
              distances += ((c.last, c.head) -> d)
            } else {
              d = d  - distances((c.head, c.last))
            }
          })
        } else {
          //println(s"NOT FOUND: ${coords.last.head} - ${coords.last.last}")
        }
      }
      else {
        //println(s"NOT FOUND2: ${path.head} ${path.last}")
      }
    })
  }

  def findDistance(x: Int, y: Int) : Option[Int] = {
    {
      for {
        m <- data
        if m.contains(x)
        z = m(x)
        if z.contains(y)
      } yield z
      }.headOption match {
      case Some(r) => Some(r(y))
      case _ => None
    }
  }

  def travers(root: Int, acc: List[Int]): List[List[Int]] = tree.filter(e => e.f == root && !acc.contains(e.t) ).toList match {
    case h::tail => travers(h.t, h.t :: acc) ++ tail.map(e => travers(e.t, e.t :: acc)).flatten
    case Nil => List(acc)
  }

  // TODO : Travers from joiner to then end of the tree with no calculated edges
  def distanceRecalc(in : List[ Matrix ], counter: Int, baseEdges: List[ List[Int]] ) : Unit = in match {
    case h::tail => {
      val f = h(counter)
      //println(s"LOG: $counter")
      //println(s"LOG: $counter")
      val res = for {
        edges <- baseEdges
      } yield {
        h(counter).filter(p => p._2 != 0)
          .map(p =>  p._1)
          .filter(e => !edges.contains(e)).map(x => edges :+ x)

      }.toList

      //res.foreach(row => println( row.mkString(" ") ) )

//      (finalKeys.toSet intersect h.keys.toSet[Int] ).foreach(joiner => {
//                for {
//                  x <- h(joiner).keys
//                } yield {
//                  val z = h(joiner)
//                  if (joiner != x)
//                    distances += ((joiner, x) -> (z(x) - delta))
//                }
//      })
//      finalKeys = h.keys.toList
      //println(h.mkString(" "))
      distanceRecalc(tail, counter - 1, res.flatten)
    }
    case Nil => Unit
  }

  //distances.foreach(row => println(s"${row._1} -> ${row._2}"))
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

    tree.append( Edge(vertexCounter, merge.head) )
    tree.append( Edge(vertexCounter, merge.last) )


    val nodesUp_ = nodesUp + vertexCounter
    vertexCounter += 1

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
