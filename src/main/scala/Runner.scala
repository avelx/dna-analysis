import com.dna.assembly.AssemblyFun._

object Runner {

  def printGraph(g: Graph) = g.foreach(row => println(row.mkString(" ")))

  def cycle(g: Graph, gf: Graph) = {

    def dive(fk: Int, tk: Int, gxk: Graph, acc: List[Int]): Array[Array[Int]] = gxk(fk)(tk) match {
      case v: Int if ((v) > 0) => {
        gxk(fk)(tk) = 0
        gxk(tk).zipWithIndex.filter(p => p._1 > 0).map(_._2) match {
          case arr: Array[Int] if (arr.length > 0) =>
            arr.map(y => dive(tk, y , gxk.clone(), tk +: acc)).flatten
          case _ =>
            Array( (tk+: acc).toArray[Int] )
        }
      }
      case _ => Array((tk +: acc).toArray[Int])
    }

    def cycleAcc(from: Int, gx: Graph): Array[Array[Int]] = {
        val r = for {
          to <- gx(from).zipWithIndex.filter(_._1 > 0).map(_._2)
          ff = dive(from, to, gx.clone(), List(from) )
        } yield ff
        r.flatten
    }

    val res = for {
      from <- 0 to 9
      paths = cycleAcc(from,  gf.clone() )
      allPaths = paths.filter(p => p.head == from)
    } yield allPaths


    //val res = cycleAcc(f,  gf )
    res.flatten.foreach(row => println(row.mkString(" ")))
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
