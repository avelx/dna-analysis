import com.dna.assembly.AssemblyFun._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random


object EulerianPath extends Profiler {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def main(args: Array[String]): Unit = {

    val gpr =
      """|0 -> 2
         |1 -> 3
         |2 -> 1
         |3 -> 0,4
         |6 -> 3,7
         |7 -> 8
         |8 -> 9
         |9 -> 6
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

    val graphAsString_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_6.txt").getLines().toList.mkString("\n")


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

    val (graph, size) = toGraph(graphAsString_)

    val nodes = new ListBuffer[Node]()
    graph.map(n => {
      val node = new Node(n.head)
      n.tail.map(nb => node.addNeigbour(nb))
      nodes.append(node)
    })

    ///////////////////////////////////////////////////////
    // PrepProcess
    val inNodes = new Array[Int](size)
    val outNodes = new Array[Int](size)
    nodes.map(n => {
      outNodes(n.i) = outNodes(n.i) + n.neighbours.length
      n.neighbours.map(nb => inNodes(nb) = inNodes(nb) + 1)
    })

    val unbalancedNodes = (0 to size - 1).map(i => if (inNodes(i) != outNodes(i)) i else 0).filter(_ > 0)
    val unbalancedFrom = unbalancedNodes.map(i => if (inNodes(i) > outNodes(i)) i else 0 ).filter(_ > 0)
    val unbalancedTo = unbalancedNodes.map(i => if (inNodes(i) < outNodes(i)) i else 0 ).filter(_ > 0)

    val unbalancedPairs = unbalancedFrom zip unbalancedTo

    unbalancedPairs.foreach(p => {
          nodes.find(_.i == p._1) match {
            case None =>
              val node = Node(p._1)
              node.addNeigbour(p._2)
              nodes.append(node)
            case Some(node) =>
              node.addNeigbour(p._2)
          }
    })

    ///////////////////////////////////////////////////////

    val numberOfEdges = graph.map(_.tail.map(_ => 1)).flatten.sum
    val circuit: Array[Int] = Array.fill(numberOfEdges + 2 )(0)
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
//          if (new_node.i == -1)
//            println(new_node)
          findCircuit(new_node)
        }
        circuit(circuitPos) = node.i
        circuitPos += 1
        false
      }
    }



//    println(unbalancedNodes.mkString(" "))
//    println()
//    unbalancedNodes.foreach(u => {
//      println(inNodes(u) + " " + outNodes(u))
//    })

    def shiftWithRemove(ubPair: List[(Int, Int)], path: List[Int]): List[Int] = ubPair match {
      case Nil => path
      case h::tail => {
        if (path.last == h._2 && path.init.last == h._1)
          shiftWithRemove(tail, path.init.init)
        else
          shiftWithRemove(ubPair, path.last +: path.init)
      }
    }

    //val index = Random.nextInt(size - 1)
    val node = nodes.find(n => n.i == unbalancedPairs.head._2).getOrElse( Node(-1) )
    val res = findCircuit(node)

    //val circuitProcessed = shiftWithRemove(unbalancedPairs.toList, circuit.reverse.init.toList)
    println( circuit.reverse.init.mkString("->") )

    //println(circuit.reverse.mkString("->"))

    /*
I found a solution inspired by the comets in the following steps:
1) identify unbalanced nodes
2) generate a dummy edge between them
3) run EulerianCycle
4) Break the cycle at the dummy edge and rearranging it.
     */
  }

}
