class TopoSort {

}
import java.util.Scanner
import scala.collection.mutable.ListBuffer

class Toposort(n: Int, m: Int, adj: Array[List[Int]]){
  val order = new ListBuffer[Int]()
  private val used = new Array[Int](adj.length)

  def run(): Unit = (0 to adj.length - 1).foreach( i => if (used(i) == 0) dfs(i) )

  private def dfs(s: Int): Unit = {
    def explore(o: List[Int]): Unit = o match {
      case h::tail =>
        if (used(h) == 0) dfs(h)
        explore(tail)
        if (used(s) == 0) {
          used(s) = 1
          order.append(s + 1)
        }
      case List() =>
    }

    if (adj(s).nonEmpty)
      explore(adj(s))
    else if (used(s) == 0) {
      used(s) = 1
      order.append(s + 1)
    }

  }
}

/*
 4 3
 1 2
 4 1
 3 1

 5 5
 1 2
 1 3
 3 4
 2 5
 4 5

7 12
1  2
1  3
1  4
2  3
2  6
3  5
3  6
3  7
4  5
4  6
5  7
6  7

6 9
1 2
1 3
1 4
1 5
1 6
2 3
2 6
5 4
5 6


4 4
1 2
1 4
2 3
3 4

 */
object Toposort {
  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in);
    val (n: Int, m: Int) = (scanner.nextInt(), scanner.nextInt())
    val adj: Array[ListBuffer[Int]] = Array.fill(n)(ListBuffer[Int]())
    for (i <- 1 to m) {
      val (x, y) = (scanner.nextInt(), scanner.nextInt())
      adj(x - 1).append(y - 1)
    }
    val top = new Toposort(n, m, adj.map(_.toList))
    top.run()
    print( top.order.reverse.mkString(" "))
  }
}