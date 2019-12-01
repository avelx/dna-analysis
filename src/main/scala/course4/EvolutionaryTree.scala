package course4

object EvolutionaryTree {

    final case class Edge(from: Int, to: Int, weight: Int)
    type Row = Array[Int]
    type Edges = Array[Edge]
    type Matrix = Array[Row]
    type Strings = Array[String]

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

}
