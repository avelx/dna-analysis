package course6

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

//HiddenMarkovModels
// https://github.com/chansonzhang/FirstNLP/blob/fcaedee100046e823b102b9c4e002169278178d4/algorithm/viterbi_algorithm.py
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
             (ts: Map[String, Double],
              es: Map[String, Double],
              ss: List[String],
              abc: List[String])(implicit debug: Boolean = false) : String = {

    val mx  = Array.ofDim[BigDecimal](in.length,ss.length)
    val back_pointer = Array.ofDim[Int](in.length,ss.length)

    // initialise
    ss.zipWithIndex.foreach(sz => {
      mx(0)(sz._2) =
        BigDecimal(es(List(sz._1, abc.head).mkString("")))
      back_pointer(0)(sz._2) = 0
    })

    // core calculations
    (1 to in.length - 1).foreach( t => {

      (0 to ss.length - 1).foreach(s => {
        mx(t)(s) = mx(t - 1)(0) *
          BigDecimal( ts( List( ss.head, ss(s) ).mkString("") ) ) *
           BigDecimal(es( List( ss(s), in(t) ).mkString("").toUpperCase() ) )

        (1 to ss.length - 1).foreach(ps => {
          if ( mx(t - 1)(ps) *
                BigDecimal( ts( List(ss(ps), ss(s) ).mkString("") ) ) *
                  BigDecimal( es( List( ss(s), in(t) ).mkString("").toUpperCase() ) )  > mx(t)(s) ){
              // set mx and back_pointers
           mx(t)(s) = mx(t - 1)(ps) *
                BigDecimal(ts( List(ss(ps), ss(s) ).mkString("") ) ) *
                  BigDecimal( es( List( ss(s), in(t) ).mkString("").toUpperCase() ) )
            back_pointer(t)(s) = ps
          }
        })

      })

    })

    var best_path_prob = mx(in.length - 1)(0)
    var best_final_state = 0
    (0 to ss.length - 1).foreach(s => {
      if ( mx( in.length - 1)(s) > best_path_prob) {
        best_final_state = s
        best_path_prob = mx( in.length - 1)(s)
      }
    })


    val res = ListBuffer[String]( ss(best_final_state) )
    var current_state : Int = best_final_state
    (0 to in.length - 1)
      .reverse
      .foreach(t => {
        current_state = back_pointer(t)(current_state)
        res.append( ss(current_state) )
    })

    if (debug){
      back_pointer.foreach(row => println(row.mkString(" ")))
    }

    val f = res
      .reverse
      .mkString("")

     f.tail
  }

  def viterbi2(obs: String)
             (transP: Map[String, Double],
              emittedP: Map[String, Double],
              states: List[String],
              startP: Map[String, Double],
              abc: List[String]) : (Double, String) = {

    val V = Array.fill(obs.length)( mutable.Map[String, Double]() )
    var path = mutable.Map[String, List[String]]()

    states.map(s => {
      V(0)(s) = startP(s) * emittedP( s"$s${obs(0)}" )
      path = path + (s -> List(s))
    })

    (1 to obs.length - 1)
      .map(t => {
        var newPath = mutable.Map[String, List[String]]()
        states
          .map(y => {
            val (prob, state) = states.map(y0 => {
              (V(t - 1)(y0) * transP(s"$y0$y") * emittedP(s"$y${obs(t)}"), y0)
            }).maxBy(_._1)
            V(t)(y) = prob
            newPath = newPath + (y -> (path(state) :+ y) )
        })
        path = newPath
      })

    val (prob, state) = states.map(y => {
      ( V( obs.length - 1)(y), y )
    }).maxBy(_._1)

    (prob, path(state).mkString("") )
  }

}
