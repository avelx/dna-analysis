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

    implicit def stringToStrings(input: String) : Strings =
        input
          .split("\n")
          .map(_.trim)
          .filter(_.nonEmpty)
          .map(_.split("//").head.trim)

    implicit def stringsToEdges(arr: Strings): Edges = {

//        val n = arr.head.toInt
        // Get list of edges
        val edges : Seq[Edge]  = for {
            a <- arr.tail
            xs = a.split("->")
            ws = xs.tail.head.split(":")
        } yield Edge( xs.head.toInt, ws.head.toInt, ws.tail.head.toInt )
        edges.toArray
    }

    /*
        Distances Between Leaves Problem: Compute the distances between leaves in a weighted tree.
        Input:  An integer n followed by the adjacency list of a weighted tree with n leaves.
        Output: An n x n matrix (di,j), where di,j is the length of the path between leaves i and j.
     */
    def leafDistance(input: String) : Matrix = {
        val arr : Strings = input
        val n: Int = arr.head.toInt
        val edges : Edges = arr

        // Number of Vertices in the Graph
        val v : Int  = edges.map(_.from).max + 1
        val dist = Array.fill(v)( Array.fill(v)(Int.MaxValue) )
        edges.foreach( e =>  dist(e.from)(e.to) = e.weight)
        (0 to v - 1).foreach(j => dist(j)(j) = 0)

        for {
            k <- 0 to v - 1
            i <- 0 to v - 1
            j <- 0 to v - 1
            if dist(i)(k) != Int.MaxValue && dist(k)(j) != Int.MaxValue
            if dist(i)(j) > dist(i)(k) + dist(k)(j)
        } yield dist(i)(j) = dist(i)(k) + dist(k)(j)

        val realDistance = dist.map(r => r.map(x => if (x == Int.MaxValue) 0 else x) )
        val output = Array.fill(n)( Array.fill(n)(0) )
        for {
            i <- 0 to n - 1
            j <- 0 to n - 1
        } yield output(i)(j) = realDistance(i)(j)
        output
    }

    def limbLength(n: Int, j: Int, matrix: Matrix) : Int = {
        val (i, k ) = ( ( j + 1) % n, (j + 2) % n)
        var minLen = (matrix(i)(j) + matrix(j)(k) - matrix(i)(k) ) / 2
        for {
            i <- 0 to n - 1
            k <- i + 1 to n - 1
            if ! (i == j || k == j)
        } yield  {
            val currLen = ( matrix(i)(j) + matrix(j)(k) - matrix(i)(k) ) / 2
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

    def findNearest(edges : Array[Array[Int]], weight: Matrix, x: Int, i: Int, k: Int): (Int, Int, Int, Int) = {
        var queue = new mutable.Queue[List[Int]]()
        queue +=  List(i)
        var visited = Set( i )
        var findPath = List[Int]()

        while( queue.length > 0){
            val path = queue.dequeue()
            val node = path.last
            visited += node
            if (node == k) {
                findPath = path
                queue.clear()
            }  else {
                for {
                    next_node <- edges(node)
                    if !visited.contains(next_node)
                } yield {
                    queue += ( path :+ next_node)
                }
            }
        }

        var dist = 0
        (0 to findPath.length - 1)
          .foldLeft( None : Option[(Int, Int, Int, Int)] )( (curr : Option[(Int, Int, Int, Int)], K) => {
            curr match {
                case None =>
                    val (i, j) = ( findPath(K), findPath(K + 1) )
                    if (dist + weight(i)(j)  > x) {
                        Some((i, j, x - dist, dist + weight(i)(j) - x))
                    } else {
                        dist += weight(i)(j)
                        curr
                    }
                case Some(_) =>
                    curr
            }
          } ).get
    }

    /*
        Code Challenge: Implement AdditivePhylogeny to solve the Distance-Based Phylogeny Problem.
        Input: An integer n followed by a space-separated n x n distance matrix.
        Output: A weighted adjacency list for the simple tree fitting this matrix.
     */
    def additivePhylogeny( D: Matrix, n: Int, inner_n: Int): ( Array[Array[Int]], Array[Array[Int]], Int) = {
        val n = D.length
        if (n == 2) {
            val edges = Array(1, 0)
            val weight : Array[Array[Int]] = Array.fill(2)( Array.fill(2)(0) )
            weight(0)(1) = D(0)(1)
            weight(1)(0) = D(0)(1)
            (edges, weight, inner_n)
        }
        val limbLen = limbLength(n, n - 1, D)
        for {
            j <- 1 to n - 1
        } yield {
            D(j)(n - 1) = D(j)(n - 1) - limbLen
            D(n - 1)(j) = D(j)(n - 1)
        }

        val (i, k) = {
            for {
                i <- 0 to n - 1
                k <- 0 to n - 1
                if D(i)(k) == D(i)(n - 1) + D(n - 1)(k)
            } yield (i, k)
        }.headOption match { case Some(a) => (a._1, a._2) case None => (-5, -5) }
        val x = D(i)(n - 1)

        val (edge, weight, inner_n_)  = additivePhylogeny( D.init.map(_.init), n - 1, inner_n )
        val (i_n, k_n, i_x, n_x) = findNearest(edge, weight, x, i, k)
        val new_node = i_n
        // need to create a new node
        if (i_x != 0 ){

        }
        val edges = edge(new_node).toBuffer
        edges.append(n - 1)
//        edges(n - 1) =
        ( Array(), Array(), 0 )
    }

}
