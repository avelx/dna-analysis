import scala.collection.mutable.ListBuffer

val transition = Map[String, Double] (
  "AA" -> 0.345,
  "AB" -> 0.655,
  "BA"-> 0.409,
  "BB"-> 0.591
)

val path = "ABAABABAABBBBBBBAAABABBABABBBBBBBAAABAAABAABBAABAB"

val prob = ListBuffer[Double]()

path
  .tail
  .fold( path.head ){ (acc, curr) =>
    val v = List(acc, curr).mkString("")
    println(v)
    prob.append(  transition(v) )
    curr
  }

def formatRes(res: Double) : BigDecimal =
  BigDecimal(res)
    .setScale(50, BigDecimal.RoundingMode.HALF_UP).toDouble

println(prob)
val res = prob
  .foldLeft(0.5D){ (acc, curr) => acc * curr }
println( formatRes(res) )