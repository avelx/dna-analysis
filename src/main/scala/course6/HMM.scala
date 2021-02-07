package course6

import scala.collection.mutable.ListBuffer

//HiddenMarkovModels
object HMM {

  def format(res: Double) : BigDecimal =
    BigDecimal(res)
      .setScale(50, BigDecimal.RoundingMode.HALF_UP).toDouble

  def probabilityOfHiddenPath(path : String)(transition: Map[String, Double] ): BigDecimal = {
    val prob = ListBuffer[Double]()
    path
      .tail
      .fold( path.head ){ (acc, curr) =>
        val v = List(acc, curr).mkString("")
        prob.append(  transition(v) )
        curr
      }

    val res = prob
      .foldLeft(0.5D){ (acc, curr) => acc * curr }

    format(res)
  }
}
