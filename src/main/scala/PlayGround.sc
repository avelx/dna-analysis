import scala.collection.mutable
import scala.collection.mutable.ListBuffer

val kmers = Seq(
  "GAGG",
  "CAGG",
  "GGGG",
  "GGGA",
  "CAGG",
  "AGGG",
  "GGAG"
)

val sp = kmers.map(s => Seq(s.init, s.tail) ).flatten
var kv = mutable.Map[String, Int]()
var i = 0
sp.map(x => {
  if (!kv.keys.exists(v => v.equals(x) )) {
    kv = kv + (x -> i)
    i += 1
  }
})

val kvInv = kv.toList.map(p => (p._2, p._1)).toMap


val size = kv.keys.toList.length
val matrix = Array.ofDim[Int]( size, size)
kmers
  .map(s => (s.init, s.tail) )
  .map(p => {
    val x = kv(p._1)
    val y = kv(p._2)
    matrix(x)(y) = matrix(x)(y) + 1
  })

val res = for{
  x <- 0 to size - 1
  y <- 0 to size - 1
  key = kvInv(x)
  if (matrix(x)(y) > 0)
  vals = Seq.fill(matrix(x)(y))(kvInv(y))
} yield (key, vals)

var r = mutable.Map[String, Seq[String]]()

res.map(p => {
  if ( r.keys.exists(x => x.equals(p._1) ))
    r = r + (p._1 -> (p._2 ++ r(p._1)) )
  else
    r = r + (p._1 -> p._2)
})

r.toList.sortBy(_._1).map(row => println(s"${row._1} -> ${row._2.mkString(",")}") )

//val res = new ListBuffer[String, String]()
//for(x <- 0 to size - 1){
//  print( kvInv(x) + "->" )
//  for(y <- 0 to size - 1){
//    if (matrix(x)(y) > 0) {
//      print(",")
//      print(Seq.fill(matrix(x)(y))(kvInv(y)).mkString(","))
//    }
//  }
//  println("")
//}
//
//matrix.foreach( row => println(row.mkString(" ")) )


//kv
//kv.zipWithIndex.foreach(p => kv + (p._1._1 -> p._2))

//println(res)
//val kmers = composition(dna, 4)
//
//val res =kmers.map(k => {
//  val bf = new ListBuffer[String]()
//  bf.append(k)
//  kmers.foreach(c => {
//    if (c != k && c.startsWith(k.tail))
//      bf.append(c)
//  })
//  bf.toList
//}).filter(l => l.length > 1)
//
//val m = Map[String, String]()
//res.map(l => {
//   if (!m.contains(l.head))
//     m + (l.head -> l.tail.mkString(",") )
//  //val s = s"${l.head} -> ${l.tail.mkString(",")}"
//  //s
//})
//
//println( m.toList.map(kv => s"${kv._1} -> ${kv._2}").mkString("\n") )