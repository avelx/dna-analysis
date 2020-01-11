package course4

object SquareError extends App {
  import course4.EvolutionaryTree._

      val d = Array(
        Array(0, 14, 17, 17),
        Array(14, 0, 13, 7),
        Array(17, 13, 0, 20),
        Array(17, 7, 20, 0)
      )

      val t = Array(
        Array(0, 15, 17, 19),
        Array(15, 0, 14, 8),
        Array(17, 14, 0, 18),
        Array(19, 8 , 18, 0)
      )

//        val d = Array(
//          Array(0, 20, 9, 11),
//          Array(20, 0, 17, 11),
//          Array(9, 17, 0, 8),
//          Array(11, 11, 8, 0)
//        )
//
//        val t = Array(
//          Array(0, 20, 8, 12),
//          Array(20, 0, 18, 12),
//          Array(8, 18, 0, 10),
//          Array(12, 12, 0, 0)
//        )

//          val d = Array(
//            Array(0, 13, 16, 10),
//            Array(13, 0, 21, 15),
//            Array(16, 21, 0, 18),
//            Array(10, 15, 18, 0)
//          )
//
//          val t = Array(
//            Array(0, 14, 17, 11),
//            Array(14, 0, 21, 15),
//            Array(17, 21, 0, 18),
//            Array(11, 15, 18, 0)
//          )

  val result = squareError(d, t)
  println(result)


}
