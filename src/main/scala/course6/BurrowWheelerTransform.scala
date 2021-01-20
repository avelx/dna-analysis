package course6

import scala.collection.mutable.ListBuffer


object BurrowWheelerTransform {


  def transform(in: String) : String = {
    def shiftRight(in : String ) : String = {
      (in.last +: in.init).mkString("")
    }


    val buff = ListBuffer[String]()
    (0 to in.length - 1 )
      .foldLeft(in)( (acc, _) => {
        val res = shiftRight(acc)
        buff.append(res)
        res
      })

    buff
      .sorted
      .map(_.last)
      .mkString("")
  }

}
