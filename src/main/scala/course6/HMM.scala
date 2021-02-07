package course6

import scala.collection.mutable.ListBuffer

//HiddenMarkovModels
object HMM {

  def format(res: Double)(implicit accuracy: Int) : BigDecimal =
    BigDecimal(res)
      .setScale(accuracy, BigDecimal.RoundingMode.HALF_UP).toDouble

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

    implicit val accuracy = 15
    format(res)
  }

  def probabilityOfOutcomeForHiddenPath(emitted: String, path: String)
                                       (emissionMatrix: Map[String, Double]) : BigDecimal = {
    val res = path
      .zip(emitted)
      .map(key => emissionMatrix(s"${key._1}${key._2}".toUpperCase()))
      .foldLeft(1D){ (acc, curr) =>
        acc * curr
      }

    BigDecimal(res)
  }


}
