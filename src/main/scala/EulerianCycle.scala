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

object EulerianCycle extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def main(args: Array[String]): Unit = {

    val gpr =
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

    val graphAsString =
      """|0 -> 1,2,3,4
         |1 -> 0,2,3,4
         |2 -> 0,1,3,4
         |3 -> 0,1,2,4
         |4 -> 0,1,2,3
      """.stripMargin

    def toGraph(s: String): (Graph, Int) = {
      val g: Graph = s
        .replaceAll("-", "")
        .replaceAll(" ", "")
        .split("\n")
        .map(_.trim())
        .map(_.split(Array(',', '>', ' ')))
        .map(_.map(_.trim))
        .map(_.map(_.toInt))
      (g, g.flatten.max + 1)
    }

    case class Node(i: Int) {
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

    val (graph, s) = toGraph(graphAsString_)

    val nodes = new ListBuffer[Node]()
    graph.map(n => {
      val node = new Node(n.head)
      n.tail.map(nb => node.addNeigbour(nb))
      nodes.append(node)
    })

    val numberOfEdges = graph.map(_.tail.map(_ => 1)).flatten.sum
    val circuit: Array[Int] = Array.fill(numberOfEdges + 1)(0)
    var circuitPos: Int = 0

    def findCircuit(node: Node): Boolean = {
      if (!node.hasNeighbours) {
        circuit(circuitPos) = node.i
        circuitPos += 1
        true
      } else {
        while (node.hasNeighbours) {
          val index = Random.nextInt(node.neighbours.size)
          val nodeId = node.neighbours(index)
          node.remove(nodeId)
          val new_node = nodes.find(n => n.i == nodeId).getOrElse(Node(-1))
          findCircuit(new_node)
        }
        circuit(circuitPos) = node.i
        circuitPos += 1
        false
      }
    }

    val index = Random.nextInt(s)
    val node = nodes(index)
    val res = findCircuit(node)

    println(circuit.reverse.mkString("->"))

  }

}
