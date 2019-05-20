val x = List(5)
val xs = List(4, 7)
x ++ xs


//val res = for {
//  x <- 1 to N - 1
//} yield x
//
//val variations = (1 to N - 1).map(s => res.slice(0, s)).toList
//
//variations.map(_.toList)
//
//
//val v = for {
//  v <- variations
//  vv = v.permutations.take(N).toList
//  kk <- vv
//  if (kk.sum == N)
//} yield  kk
//
//v.foreach(println(_))

//val N : Int = 4
//val base = Array.fill(N)(1)
//
//val buff = base.toBuffer
//
//val t = for {
//    x <- 2 to N - 2
//    f = x +: base.takeRight(N - x).toList
//  } yield f
//
//val data = (base.toList +: t).toList
//
//val e = data.map(_.permutations.take(N - 1).toList)
//
//e.foreach(row => println(row.mkString(" ")))
//
//val count = (x: Int) => x * (x + 1) / 2 + 1
//
//val r = count(18092)