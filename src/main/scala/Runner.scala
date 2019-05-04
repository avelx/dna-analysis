import com.dna.assembly.AssemblyFun._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

trait Profiler {
  def time[R](block: => R): R = {
    val start = System.currentTimeMillis()
    val result = block
    val end = System.currentTimeMillis()
    println(s"Elapsed time: ${end - start} ms")
    result
  }
}

object Runner extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def cycle(g: Graph, size: Int) = {

    val gf: Graph = Array.ofDim[Int](size, size)
    g.map(row => {
      val x = row.head
      row.tail.map(y => gf(x)(y) = gf(x)(y) + 1)
    })


    def dive(fk: Int, tk: Int, gxk: Graph, acc: List[Int], baseFrom: Int): Array[Array[Int]] = gxk(fk)(tk) match {
      case v: Int if ((v) > 0) => {
        gxk(fk)(tk) = 0
        gxk(tk).zipWithIndex.filter(p => p._1 > 0).map(_._2) match {
          case arr: Array[Int] if (arr.length > 0) =>
          {
            for {
              y <- arr
              r = dive(tk, y, gxk, tk +: acc, baseFrom)
            } yield r
          }.flatten
          case _ =>
            val out = (tk +: acc).toArray[Int]
            if (out.head == baseFrom) Array(out) else Array()
        }
      }
      case _ =>
        val out = (tk +: acc).toArray[Int]
        if (out.head == baseFrom) Array(out) else Array()
    }

    def cycleAcc(from: Int, gx: Graph): Array[Array[Int]] = {
      val r = for {
        to <- gx(from).zipWithIndex.filter(_._1 > 0).map(_._2)
        ff = dive(from, to, gx, List(from), from)
      } yield ff
      r.flatten
    }

    val numberOfEdges = g.map(_.tail.map(_ => 1)).flatten.sum

    var pathsResult = Array[Array[Int]]()
    var from = -1
    var froms = mutable.Set[Int]()
    while (!pathsResult.exists(res => res.length == numberOfEdges + 1) && froms.size != size) {
      do{
        from = Random.nextInt(size)
      } while (froms.contains(from))
      froms = froms + from
      println(s"From: $from - Left: ${size - froms.size}")
      val paths = cycleAcc(from, gf.map(_.clone()))
      pathsResult = paths.filter(p => p.head == from)
    }

    if (pathsResult.exists(res => res.length == numberOfEdges + 1)) {
      println(pathsResult.head.reverse.mkString("->"))
    }

    //val res = cycleAcc(f,  gf )
    //pathsResult.foreach(row => println( row.mkString("->") ) )
    // Find Graph
    //val t = res.find(row => row.length > 1 && row.last == f).getOrElse( Array() )
    //println( t.mkString(" ") )

  }


  def main(args: Array[String]): Unit = {

    val graphAsString =
      """|0 -> 3
         |1 -> 0
         |2 -> 1,6
         |3 -> 2
         |4 -> 2
         |5 -> 4
         |6 -> 5,8
         |7 -> 9
         |8 -> 7
         |9 -> 6
      """.stripMargin

    def toGraph(s: String): (Graph, Int) = {
      val g: Graph = s
        .replaceAll("-", "")
        .replaceAll(" ", "")
        .split("\n")
        //.map(_.split("//"))
        //.map(_.head)
        .map(_.trim())
        .map(_.split(Array(',', '>', ' ')))
        .map(_.map(_.trim))
        .map(_.map(_.toInt))
      (g, g.flatten.max + 1)
    }

    case class Node(i: Int){
      def hasNeighbours: Boolean = neighbours.size > 0
      val neighbours = ListBuffer[Int]()
      def addNeigbour(i: Int) = neighbours.append(i)
      def remove(i: Int) = {
        val index = neighbours.indexOf(i)
        if (index == -1)
          throw new Error("Index not exists")
        else
          neighbours.remove(index)
      }
    }


    val graphAsString_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_2.txt").getLines().toList.mkString("\n")

    val (graph, s) = toGraph(graphAsString)

    val nodes = new ListBuffer[Node]()
    graph.map(n => {
      val node = new Node(n.head)
      n.tail.map(nb => node.addNeigbour(nb))
      nodes.append(node)
    })

    val numberOfEdges = graph.map(_.tail.map(_ => 1)).flatten.sum
    val circuit : Array[Int] = Array.fill(numberOfEdges + 1)(0)
    var circuitPos : Int = 0

    def find_cicuit(node: Node): Boolean = {
      if (!node.hasNeighbours) {
        circuit(circuitPos) = node.i
        circuitPos += 1
        true
      } else {
        while(node.hasNeighbours){
          val index = Random.nextInt(node.neighbours.size)
          val nodeId = node.neighbours(index)
          node.remove(nodeId)
          val new_node = nodes.find(n => n.i == nodeId).getOrElse( Node(-1))
          find_cicuit(new_node)
        }
        circuit(circuitPos) = node.i
        circuitPos += 1
        false
      }
    }

    val res = find_cicuit( nodes.head )

    println( circuit.mkString(" ") )

    println(res)

    //printGraph(g)
    //println(edges)





    //val res = cycle(gh, s)

    //println(res)

    //val res = time {
    //  eulerianCycle(gh, s)
    //}

  }

}
