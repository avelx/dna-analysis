import com.dna.assembly.AssemblyFun._

import scala.io.Source

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

  def cycle(g: Graph, gf: Graph) = {

    def dive(fk: Int, tk: Int, gxk: Graph, acc: List[Int]): Array[Array[Int]] = gxk(fk)(tk) match {
      case v: Int if ((v) > 0) => {
        gxk(fk)(tk) = 0
        gxk(tk).zipWithIndex.filter(p => p._1 > 0).map(_._2) match {
          case arr: Array[Int] if (arr.length > 0) =>
            arr.map(y => dive(tk, y, gxk.clone(), tk +: acc)).flatten
          case _ =>
            Array((tk +: acc).toArray[Int])
        }
      }
      case _ => Array((tk +: acc).toArray[Int])
    }

    def cycleAcc(from: Int, gx: Graph): Array[Array[Int]] = {
      val r = for {
        to <- gx(from).zipWithIndex.filter(_._1 > 0).map(_._2)
        ff = dive(from, to, gx.clone(), List(from))
      } yield ff
      r.flatten
    }

    //    val res = for {
    //      from <- 0 to 9
    //      paths = cycleAcc(from,  gf.clone() )
    //      allPaths = paths.filter(p => p.head == from)
    //    } yield allPaths

    val edgesNumber = g.map(_.tail.map(_ => 1)).flatten.sum

    var pathsResult = Array[Array[Int]]()
    var from: Int = 0
    while (!pathsResult.exists(res => res.length == edgesNumber - 1) && from < gf.length) {
      val paths = cycleAcc(from, gf.map(_.clone()))
      pathsResult = paths.filter(p => p.head == from)
      from += 1
    }


    println(pathsResult.flatten.reverse.mkString("->"))

    //val res = cycleAcc(f,  gf )
    //pathsResult.foreach(row => println( row.mkString("->") ) )
    // Find Graph
    //val t = res.find(row => row.length > 1 && row.last == f).getOrElse( Array() )
    //println( t.mkString(" ") )

  }


  def main(args: Array[String]): Unit = {

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
        //.map(_.split("//"))
        //.map(_.head)
        .map(_.trim())
        .map(_.split(Array(',', '>', ' ')))
        .map(_.map(_.trim))
        .map(_.map(_.toInt))
      (g, g.flatten.max + 1)
    }


    //cycle(g, matrix)
    //printGraph(g)
    //println(edges)

    val graphAsString_ = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/data/dataset_203_2.txt").getLines().toList.mkString("\n")
    val (gh, s) = toGraph(graphAsString_)

    val res = time {
      eulerianCycle(gh, s)
    }
    println(res)

  }

}
