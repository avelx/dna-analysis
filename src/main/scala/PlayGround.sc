import scala.collection.mutable.ListBuffer

val res = "20, 20, 30, 30, 60, 60, 80, 200"

val contings = res
  .replace(",", "")
  .split(" ")
  .map(_.toInt)


def n50(a: Array[Int]): Int = {
  //val sum50 = a.sum * 0.75
  val sum50 = 500
  val top = a.reverse.foldLeft( List[Int]() )( (acc, curr) => if (acc.sum <= sum50 ) acc :+ curr else acc )
  println( top.mkString(" ") )
  top.lastOption.getOrElse(0)
}

n50( contings )