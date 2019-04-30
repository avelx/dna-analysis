import com.dna.assembly.AssemblyFun._

import scala.collection.mutable.ListBuffer


object Runner {


  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def cycle(g: Graph, gf: Graph) = {

    def dive(fk: Int, tk: Int, gxk: Graph, acc: List[Int]): Array[Int] = gxk(fk)(tk) match {
      case v: Int if ((v) > 0) => {
        gxk(fk)(tk) = 0
        gxk(tk).zipWithIndex.find(p => p._1 > 0).map(_._2) match {
          case Some(y) =>
            dive(tk, y , gxk, tk +: acc)
          case None =>
            acc.toArray[Int]
        }
      }
      case _ => acc.toArray[Int]
    }

    def cycleAcc(from: Int, gx: Graph): Array[Array[Int]] = {
        val r = for {
          to <- gx(from).zipWithIndex.filter(_._1 > 0).map(_._2)
          ff = dive(from, to, gx.clone(), List(from) )
        } yield ff
        r
    }

    //    def cycleAcc(from: Int, path: List[Int], gx: Graph, baseFrom: Int): Array[Array[Int]] = g(from) match {
//      case a: Array[Int] => {
//        val ax = a.map(y => {
//          if (gx(from)(y) > 0) {
//            gx(from)(y) = gx(from)(y) - 1
//            cycleAcc(y, path :+ y, gx, baseFrom)
//          } else
//            Array(path.toArray[Int])
//        })
//        ax.flatten
//      }
//    }

    val res =for {
      from <- 0 to 9
      paths = cycleAcc(from,  gf.clone() )
      real = paths.find(p => p.last == from)
      if (real != None)
    } yield real.getOrElse( Array() )

    //val res = cycleAcc(f,  gf )
    res.foreach(row => println(row.mkString(" ")))
    // Find Graph
    //val t = res.find(row => row.length > 1 && row.last == f).getOrElse( Array() )
    //println( t.mkString(" ") )

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
