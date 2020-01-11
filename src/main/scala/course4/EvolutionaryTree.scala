package course4


import scala.collection.mutable
import scala.collection.mutable.ListBuffer


object EvolutionaryTree {

  final case class Edge(from: Int, to: Int, weight: Int)

  type Row = Array[Int]
  type Edges = Array[Edge]
  type Matrix = Array[Row]
  type Strings = Array[String]
  type Tree = Edges

  type Min = Int
  type Chracter = Int

  implicit def stringToStrings(input: String): Strings =
    input
      .split("\n")
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.split("//").head.trim)

  implicit def stringsToEdges(arr: Strings): Edges = {

    //        val n = arr.head.toInt
    // Get list of edges
    val edges: Seq[Edge] = for {
      a <- arr.tail
      xs = a.split("->")
      ws = xs.tail.head.split(":")
    } yield Edge(xs.head.toInt, ws.head.toInt, ws.tail.head.toInt)
    edges.toArray
  }

  /*
      Distances Between Leaves Problem: Compute the distances between leaves in a weighted tree.
      Input:  An integer n followed by the adjacency list of a weighted tree with n leaves.
      Output: An n x n matrix (di,j), where di,j is the length of the path between leaves i and j.
   */
  def leafDistance(input: String): Matrix = {
    val arr: Strings = input
    val n: Int = arr.head.toInt
    val edges: Edges = arr

    // Number of Vertices in the Graph
    val v: Int = edges.map(_.from).max + 1
    val dist = Array.fill(v)(Array.fill(v)(Int.MaxValue))
    edges.foreach(e => dist(e.from)(e.to) = e.weight)
    (0 to v - 1).foreach(j => dist(j)(j) = 0)

    for {
      k <- 0 to v - 1
      i <- 0 to v - 1
      j <- 0 to v - 1
      if dist(i)(k) != Int.MaxValue && dist(k)(j) != Int.MaxValue
      if dist(i)(j) > dist(i)(k) + dist(k)(j)
    } yield dist(i)(j) = dist(i)(k) + dist(k)(j)

    val realDistance = dist.map(r => r.map(x => if (x == Int.MaxValue) 0 else x))
    val output = Array.fill(n)(Array.fill(n)(0))
    for {
      i <- 0 to n - 1
      j <- 0 to n - 1
    } yield output(i)(j) = realDistance(i)(j)
    output
  }

  def limbLength(n: Int, j: Int, matrix: Matrix): Int = {
    val (i, k) = ((j + 1) % n, (j + 2) % n)
    var minLen = (matrix(i)(j) + matrix(j)(k) - matrix(i)(k)) / 2
    for {
      i <- 0 to n - 1
      k <- i + 1 to n - 1
      if !(i == j || k == j)
    } yield {
      val currLen = (matrix(i)(j) + matrix(j)(k) - matrix(i)(k)) / 2
      minLen = if (minLen > currLen) currLen else minLen
    }
    minLen
  }

  /*
  Data:
  edge:       {0: [1], 1: [0]}
  weight:     {(0, 1): 13, (1, 0): 13}
  x: 2
  i: 1
  k: 0
  Result:
  i_near 4 k_near = 0 i_x 2 n_x 11

   */

  def findNearest(edges: Map[Int, List[Int]], weight: Map[ (Int, Int), Int], x: Int, i: Int, k: Int): (Int, Int, Int, Int) = {
    var queue = new mutable.Queue[List[Int]]()
    queue += List(i)
    var visited = Set(i)
    var findPath = List[Int]()

    while (queue.length > 0) {
      val path = queue.dequeue()
      val node = path.last
      visited += node
      if (node == k) {
        findPath = path
        queue.clear()
      } else {
        for {
          next_node <- edges(node)
          if !visited.contains(next_node)
        } yield {
          queue += (path :+ next_node)
        }
      }
    }

    var dist = 0
    (0 to findPath.length - 1)
      .foldLeft(None: Option[(Int, Int, Int, Int)])((curr: Option[(Int, Int, Int, Int)], K) => {
        curr match {
          case None =>
            val (i, j) = (findPath(K), findPath(K + 1))
            if (dist + weight( (i,j) ) > x) {
              Some((i, j, x - dist, dist + weight( (i,j) ) - x))
            } else {
              dist += weight(i,j)
              curr
            }
          case Some(_) =>
            curr
        }
      }).get
  }

  def find(D: Matrix, n: Int): Option[(Int, Int)] = {
    {
      for {
        i <- 0 to n - 1
        k <- 0 to n - 1
        if D(i)(k) == D(i)(n - 1) + D(n - 1)(k)
      } yield (k, i)
      }.headOption
  }

  class ListExtension[T](a: List[T]) {
    def -(v: T): List[T] = {
      def dropValAcc(in: List[T], acc: List[T]) : List[T] = in match {
        case Nil => acc
        case h::tail => if (h == v) dropValAcc(tail, acc) else dropValAcc(tail, acc :+ h)
      }
      dropValAcc(a, List() )
    }
  }

  implicit def listToListExtension[T](l : List[T]) = new ListExtension[T](l)


  /*
      Code Challenge: Implement AdditivePhylogeny to solve the Distance-Based Phylogeny Problem.
      Input: An integer n followed by a space-separated n x n distance matrix.
      Output: A weighted adjacency list for the simple tree fitting this matrix.
   */
  def additivePhylogeny(D: Matrix, n: Int, inner_n: Int): (Map[Int, List[Int]], Map[(Int,Int), Int], Int) = {
    if (n == 2)
      return ( Map[ Int, List[Int]](0 -> List(1), 1 -> List(0)),
               Map[ (Int, Int), Int]((0,1) -> D(0)(1), (1, 0) -> D(0)(1)),
               inner_n)
    val limbLen = limbLength(n, n - 1, D)
    (0 to n - 1).foreach(i  => {
      D(i)(n - 1) = if (D(i)(n - 1) - limbLen >= 0) D(i)(n - 1) - limbLen else 0
      D(n - 1)(i) = if (D(n - 1)(i) - limbLen >= 0) D(n - 1)(i) - limbLen else 0
    })
    val (i, k) = find(D, n).get
    var (edge, weight, inner_n_) = additivePhylogeny(D.init.map(_.init), n - 1, inner_n)
    val (i_near, k_near, i_x, n_x) = findNearest(edge, weight, D(i)(n - 1), i, k)
    var new_node = i_near
    if (i_x != 0) {
      new_node = inner_n_
      edge = edge + (i_near -> ( edge(i_near) - k_near ) )
      edge = edge + (k_near -> ( edge(k_near) - i_near ) )
      edge = edge + (i_near -> ( edge(i_near) :+ new_node ) )
      edge = edge + (k_near -> ( edge(k_near) :+ new_node) )
      edge = edge + (new_node -> List(i_near, k_near) )
      weight = weight + ( (new_node, i_near) -> i_x )
      weight = weight + ( (i_near, new_node) -> i_x )
      weight = weight + ( (new_node, k_near) -> n_x )
      weight = weight + ( (k_near, new_node) -> n_x )
      weight = weight - ( (i_near, k_near) )
      weight = weight - ( (k_near, i_near) )
    }
    edge = edge + (new_node -> (edge(new_node) :+ (n - 1)) )
    edge = edge + ( (n - 1)  -> List(new_node) )
    weight = weight + ((n - 1, new_node) ->  limbLen)
    weight = weight + ((new_node, n - 1) ->  limbLen)
    (edge, weight, if (i_x != 0) inner_n_ + 1 else inner_n_)
  }

  def argmin_S(M: Array[Array[Float]]) : Int = {
    case class P( element: Float, index: Int, found: Int )
    M.flatten.foldLeft( P(Int.MaxValue,0, 0) )( (curr, e) =>
      if (e != 0.0F && curr.element > e) P(e, curr.index + 1, curr.index)
      else  P(curr.element, curr.index + 1, curr.found) )
      .found
  }

  def upgma(d: Matrix, n: Int) : Array[Array[Int]] = {

    import breeze.linalg._
    import breeze.numerics._

    def delete[T](l: List[T], idx: Array[Int]) : List[T] =
      idx.foldLeft(l)( (result, index) => result.take(index) ++ result.drop(index + 1))

    val D =  DenseMatrix( d.map(_.map(_.toFloat)) :_*)

    var clusters : Array[Array[Int]] = {
      for {
        i <- 0 to n - 1
      } yield Array(i, 1)
    } toArray

    val adj : ListBuffer[Array[Int]] = ListBuffer.fill(n)( Array[Int]() )
    val age = ListBuffer.fill(n)(0.0F)

    if (D.size <= 1)
      return adj.toArray

    var stop = false
    while (!stop){
      val index = argmin(D)._1
      val i : Int = index / D.size
      val j : Int = index % D.size
      val i_new = adj.length
      adj.append( Array() )
      val C_new = Array(i_new, clusters(i)(1) + clusters(j)(1) )
      adj(i_new) = adj(i_new) :+ clusters(i)(0)
      adj(i_new) = adj(i_new) :+ clusters(j)(0)

      adj(clusters(i)(0)) =  adj(clusters(i)(0)) :+ i_new
      adj(clusters(j)(0)) =  adj(clusters(i)(0)) :+ i_new

      age.append( D(i, j) / 2)

      if (D.size == 2)
        stop = true

//      var d_new = {
        val A =  D(i, ::)
        val d = clusters(i)(1) + clusters(j)(1)

//        Array()
//      }.toList
//      d_new = delete(d_new, Array(i, j) )

//      D(0) = delete(D(0).toList, Array(i, j)).toArray
//      D(1) = delete(D(1).toList, Array(i, j)).toArray
//
//      D = D :+ d_new.toArray
//      d_new = d_new :+ Float.MaxValue
//      D = D :+ d_new.toArray
//
//      if (i < j) {
//        clusters = delete( clusters.toList, Array(j) ).toArray
//        clusters = delete( clusters.toList, Array(i) ).toArray
//      } else {
//        clusters = delete( clusters.toList, Array(i) ).toArray
//        clusters = delete( clusters.toList, Array(j) ).toArray
//      }
//
//      clusters = clusters :+ C_new
    }

    Array()
  }

  def squareError(d : Array[Array[Int]], t: Array[Array[Int]]): Int ={
//    val d = Array(
//      Array(0, 13, 16, 10),
//      Array(13, 0, 21, 15),
//      Array(16, 21, 0, 18),
//      Array(10, 15, 18, 0)
//    )
//
//    val t = Array(
//      Array(0, 14, 17, 11),
//      Array(14, 0, 21, 15),
//      Array(17, 21, 0, 18),
//      Array(11, 15, 18, 0)
//    )

    val discr = for {
      i <- 0 to 3
      j <- 0 to 3
      if i < j
    } yield {
      val y = t(i)(j) - d(i)(j)
      y * y
    }
    discr.toList.sum
  }

  def smallParsimony(T: Tree, C: Chracter) : Min = 5
}
