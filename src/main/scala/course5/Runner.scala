package course5

object Runner extends App {
  import course5.Cluster._

  val s = "00110010"
  val t = "11001111"

  val (nominator, denominator) = diff(s, t)
  println( nominator.toDouble / denominator.toDouble )


// Task 5
//  val column = "11101"
//  val v = Seq(0, 1)
//  val allVectors = for {
//    a <- v
//    b <- v
//    c <- v
//    d <- v
//    e <- v
//  } yield Seq(a, b, c, d, e).mkString("")
//
//  val res = for {
//    candidate <- allVectors
//    r = subset(column, candidate) || subset(candidate, column) || disjoint(candidate, column)
//  } yield  if (r) 1 else 0
//
//  val s = res.sum
//  println(s)

//  val r = subset("110000", "100000")
//  println(r)

//  allVectors.foreach(r => println(r))
  //println(allVectors.length)

  /* Task#2 */
  //  val data = Array( Point(Seq(2, 8)), Point( Seq(2, 5) ), Point( Seq(6, 9) ),
  //    Point( Seq( 7, 5) ), Point( Seq(5, 2) ) )
  //  val centers = Array( Point( Seq(3, 5) ), Point( Seq(5, 4) ) )
  //  val hm = hiddenMatrix(1, 4, data, centers)
  //  println(hm)

  /* Task 3 */
  //  val data = Array( Point( Seq(2, 6) ), Point( Seq(4, 9) ), Point( Seq(5, 7) ),
  //    Point( Seq(6, 5)  ), Point( Seq(8, 3) ) )
  //
  //  val hm = Array(
  //    Array(0.6, 0.1, 0.8, 0.5, 0.7),
  //    Array(0.4, 0.9, 0.2, 0.5, 0.3)
  //  )

  //  val data = Array( Point( Seq(2, 8) ), Point( Seq(2, 5) ), Point( Seq(6, 9) ),
  //    Point( Seq(7, 5)  ), Point( Seq(5, 2) ) )
  //  val hm = Array(
  //    Array(0.5, 0.3, 0.8, 0.4, 0.9),
  //    Array(0.5, 0.7, 0.2, 0.6, 0.1)
  //  )

  //  val res = computeWeightedCenterGravity(1, data, hm)
  //  println(res)

  /* Task 5 */
  //  val data = Array(
  //    Array(0, 20, 9, 11),
  //    Array(20, 0, 17, 11),
  //    Array(9, 17, 0, 8),
  //    Array(11, 11, 8, 0)
  //  )
  //  val m = Seq ( dmin( Seq(0,3),  data), dmin( Seq(1,2),  data) ).min
  //  println(m)
  //    val ag = davg( Seq(0, 3, 1, 2),  data)
  //    println(ag)


  //  val data = Seq(Point(Seq(2, 6)), Point(Seq(4, 9)),
  //    Point(Seq(5, 7)), Point(Seq(6, 5)), Point(Seq(8, 3)))
  //
  //  val centers = Seq(Point(Seq(4, 5)), Point(Seq(7, 4)))
  //
  //  val actual = maxDistance(data, centers)._1
  //
  //  println(actual)

  //    val data = Seq( Point( Seq(2, 8) ), Point( Seq(2, 5) ),
  //      Point( Seq(6, 9) ), Point( Seq(7, 5) ), Point( Seq(5, 2) ) )
  //
  //    val centers = Seq( Point( Seq(3, 5) ), Point( Seq(5, 4) )  )
  //
  //    val actual = distortion(data, centers)
  //
  //    println(actual)
}
