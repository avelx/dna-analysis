import com.dna.assembly.AssemblyFun._

import scala.collection.mutable.ListBuffer


object Runner {


  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def cycle(g: Graph, gf: Graph) = {

    val paths = new ListBuffer[Array[Int]]()

    def cycleAcc(from: Int, path: List[Int], gx: Graph, baseFrom: Int): Array[Array[Int]] = g(from) match {
      case a: Array[Int] => {
        val ax = a.map(y => {
          if (gx(from)(y) > 0) {
            gx(from)(y) = gx(from)(y) - 1
            cycleAcc(y, path :+ y, gx, baseFrom)
          } else
            Array(path.toArray[Int])
        })
        ax.flatten
      }
    }

    val f = 0
    val res = cycleAcc(f, List(0), gf, f)
    res.foreach(row => println(row.mkString(" ")))
    //println( res.mkString(" ") )
  }


  def main(args: Array[String]): Unit = {

    val g: Graph =
      """0 -> 3
        |     1 -> 0
        |     2 -> 1,6
        |     3 -> 2
        |     4 -> 2
        |     5 -> 4
        |     6 -> 5,8
        |     7 -> 9
        |     8 -> 7
        |     9 -> 6""".stripMargin
        .replaceAll("-", "")
        .replaceAll(" ", "")
        .split("\n")
        //.map(_.split("//"))
        //.map(_.head)
        .map(_.trim())
        .map(_.split(Array(',', '>', ' ')))
        .map(_.map(_.trim))
        .map(_.map(_.toInt))

    val size = 10
    // from / to
    val matrix: Graph = Array.ofDim[Int](size, size)
    g.map(row => {
      val x = row.head
      row.tail.map(y => matrix(x)(y) = matrix(x)(y) + 1)
    })

    cycle(g, matrix)
    //printGraph(matrix)
  }

}
