package course6

import scala.collection.mutable
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

  def viterbi(in: String)
             (transition: Map[String, Double],
              emission: Map[String, Double],
              states: List[String]) : String = {

    val matrix  = Array.ofDim[Double](in.length,states.length)
    val back_pointer = Array.ofDim[Int](in.length,states.length)

    // initialise
    (0 to in.length - 1).foreach(i => {
      states.zipWithIndex.foreach(jp => {
        matrix(i)(0) = emission( List(jp._1, "X").mkString("") )
      })
    })

    // core calculations
    (1 to in.length - 1).foreach( t => {

      (0 to states.length - 1).foreach(s => {
        matrix(t)(s) = matrix(t - 1)(0) *
          transition( List("A", states(s) ).mkString("") ) *
            emission( List( states(s), in(t) ).mkString("").toUpperCase() )

        (1 to states.length - 1).foreach(ps => {
          if ( matrix(t - 1)(ps) *
                transition( List(states(ps), states(s) ).mkString("") ) *
                  emission( List( states(s), in(t) ).mkString("").toUpperCase() ) > matrix(t)(s) ){
              // set matrix and back_pointers
              matrix(t)(s) = matrix(t - 1)(ps) *
                transition( List(states(ps), states(s) ).mkString("") ) *
                  emission( List( states(s), in(t) ).mkString("").toUpperCase() )
            back_pointer(t)(s) = ps
          }
        })

      })

    })

    var best_path_prob = matrix(in.length - 1)(0)
    var best_final_state = 0
    (0 to states.length - 1).foreach(s => {
      if ( matrix( in.length - 1)(s) > best_path_prob) {
        best_path_prob = matrix( in.length - 1)(s)
        best_final_state = s
      }
    })

    val res = ListBuffer[String]()
    var current_state : Int = best_final_state
    (0 to in.length - 1)
      .reverse
      .foreach(t => {
        current_state = back_pointer(t)(current_state)
        res.append( states(current_state) )
    })

    val f = res
      .reverse
      .mkString("")

   f.tail :+ f.head
  }
}
