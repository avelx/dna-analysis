package course4

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try


object EvolutionaryTree {

  final case class Edge(from: Int, to: Int, weight: Int)

  type Row = Array[Int]
  type Edges = Array[Edge]
  type Matrix = Array[Row]
  type Strings = Array[String]
  type Tree = Edges

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

  def findNearest(edges: Map[Int, Array[Int]], weight: Map[ (Int, Int), Int], x: Int, i: Int, k: Int): (Int, Int, Int, Int) = {
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

  /*
      Code Challenge: Implement AdditivePhylogeny to solve the Distance-Based Phylogeny Problem.
      Input: An integer n followed by a space-separated n x n distance matrix.
      Output: A weighted adjacency list for the simple tree fitting this matrix.
   */
  def additivePhylogeny(D: Matrix, n: Int, inner_n: Int): (Map[Int, Array[Int]], Map[(Int,Int), Int], Int) = {

    def dropVal[T](a: List[T], v: Int): List[T] = {
      def dropValAcc(in: List[T], acc: List[T]) : List[T] = in match {
        case Nil => acc
        case h::tail => if (h == v) dropValAcc(tail, acc) else dropValAcc(tail, acc :+ h)
      }
      Try{ dropValAcc(a, List() ) }.toOption.getOrElse({
        println("Got error!")
        a
      })
    }

    if (n == 2) {
      var edges = Map[ Int, Array[Int]]()
      edges = edges + (0 -> Array(1))
      edges = edges + (1 -> Array(0) )
      var weight  = Map[ (Int, Int), Int]()
      weight = weight + ( (0,1) -> D(0)(1) )
      weight = weight + ( (1, 0) -> D(0)(1) )
      return (edges, weight, inner_n)
    }
    val limbLen = limbLength(n, n - 1, D)

    for {
      i <- 0 to n - 1
    } yield {
      D(i)(n - 1) = if ( D(i)(n - 1) - limbLen >= 0) D(i)(n - 1) - limbLen else 0
      D(n  - 1)(i) = if ( D(n  - 1)(i) - limbLen >= 0) D(n  - 1)(i) - limbLen else  0
    }

    val (i, k) = find(D, n).getOrElse( (-5, -5) )
    val x = D(i)(n - 1)

    var (edge, weight, inner_n_) = additivePhylogeny(D.init.map(_.init), n - 1, inner_n)
    var (i_near, k_near, i_x, n_x) = findNearest(edge, weight, x, i, k)
    var new_node = i_near

    if (i_x != 0) {
      new_node = inner_n_
      inner_n_ = inner_n_ + 1

      edge = edge + (i_near -> dropVal( edge(i_near).toList, k_near ).toArray.distinct )
      edge = edge + (k_near -> dropVal( edge(k_near).toList, i_near ).toArray.distinct )
      edge = edge + (i_near -> ( edge(i_near).toList :+ new_node ).toArray.distinct )
      edge = edge + (k_near -> ( edge(k_near).toList :+ new_node).toArray.distinct )
      edge = edge + (new_node -> Array(i_near, k_near) )

      weight = weight + ( (new_node, i_near) -> i_x )
      weight = weight + ( (i_near, new_node) -> i_x )
      weight = weight + ( (new_node, k_near) -> n_x )
      weight = weight + ( (k_near, new_node) -> n_x )
      weight = weight - ( (i_near, k_near) )
      weight = weight - ( (k_near, i_near) )

    }

    edge = edge + (new_node -> (edge(new_node).toList :+ (n - 1)).toArray )
    edge = edge + ( (n - 1)  -> Array(new_node) )

    weight = weight + ((n - 1, new_node) ->  limbLen)
    weight = weight + ((new_node, n - 1) ->  limbLen)

    (edge, weight, inner_n_)
  }

}
