def dropIndex[T](a: List[T], index: Int): List[T] = {
  def dropIndexAcc(in: List[T], acc: List[T], p: Int) : List[T] = in match {
    case Nil => acc
    case h::tail =>
      if (p != index) dropIndexAcc(tail, acc :+ h, p + 1)
      else dropIndexAcc(tail, acc, p + 1)
  }
  dropIndexAcc(a, List(), 0)
}


val a = List(1, 2, 3, 4, 5)
val r = dropIndex[Int](a, 2)
println(r)


//val input =
//  """
//    |4
//    |0->4:11
//    |1->4:2
//    |2->5:6
//    |3->5:7
//    |4->0:11
//    |4->1:2
//    |4->5:4
//    |5->4:4
//    |5->3:7
//    |5->2:6
//    |""".stripMargin
//
//
//val arr = input
//  .split("\n")
//  .map(_.trim)
//  .filter(_.nonEmpty)
//  .map(_.split("//").head.trim )
//  .tail
//
//val n = arr.head.toInt
//
//
