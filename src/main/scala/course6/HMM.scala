package course6

import scala.collection.mutable.ListBuffer
import scala.util.Try

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

  def viterby(in: String)
             (stateTransition: Map[String, Double],
              emissionMatrix: Map[String, Double],
              states: List[String]) : String = {
    val res = ListBuffer[String]()
    in.foldLeft(""){ (prev, curr) =>
      val k : Double = Try(
        stateTransition( List(prev, curr).mkString(""))
      ).toOption.getOrElse(1D)

      val ls = states.map(state => (state,
        k * emissionMatrix( List(state, curr)
          .mkString("").toUpperCase ) ) )
      val mx = ls.maxBy(_._2)
      res.append(mx._1)
      mx._1
    }
    res.mkString("")
  }
}
