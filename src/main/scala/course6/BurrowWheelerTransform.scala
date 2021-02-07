package course6

import scala.collection.mutable.ListBuffer

object BurrowWheelerTransform {

  def suffixArray(in: String) : List[Int] = {
    val res = {
      for {
        r <- 0 to in.length - 1
      } yield in
        .reverse
        .substring(0, r + 1)
        .reverse
    }.toList

    println( res )
    val r2 = in
      .zipWithIndex
      .map(_._2).reverse
      .zip(res)

    println(r2)

    r2
      .sortBy(_._2)
      .map(_._1)
      .toList
  }

  private def shiftLeft(in : String ) : String = {
    ( in.tail ++ List(in.head)).mkString("")
  }

  def transform(in: String) : String = {
    val buff = ListBuffer[String]()
    (0 to in.length - 1 )
      .foldLeft(in)( (acc, _) => {
        val res = shiftLeft(acc)
        buff.append(res)
        res
      })

//    println( buff mkString("\n") )

    buff
      .sorted
      .map(_.last)
      .mkString("")
  }

  def transformBack(encode: String) : String = {
    val head = encode.sorted.zipWithIndex
    val last = encode.zipWithIndex

    def charOccurance(c: Char, index: Int, occured: Int, acc: List[(Char, Int)] ) : Int = acc match {
      case h::_ if (h._1 == c && h._2 == index) =>
        occured + 1
      case h::tail if h._1 == c && h._2 <= index => charOccurance(c, index, occured + 1, tail)
      case _::tail => charOccurance(c, index, occured, tail)
      case Nil =>
        occured
    }

    def rowIndexInOccurance( c: Char, occurance: Int, acc: List[(Char, Int)] ) : Int = acc match {
      case h::tail if h._1 == c && occurance == 0 =>
        h._2 + 1
      case h::tail if h._1 == c && occurance > 0 =>
        if (occurance == 1)
          h._2 + 1
        else
          rowIndexInOccurance(c, occurance - 1, tail)
      case _::tail =>
        rowIndexInOccurance(c, occurance, tail)
      case Nil =>
        -1
    }

    //println( rowIndexInOccurance( 'd', 1, last.toList))

    val headCharOccurance = head
      .map(p => (p, charOccurance(p._1, p._2, 0, head.toList) ) )
      .toList

    def reconstruct(c: Char, occurance: Int, acc: List[Char]) : List[Char] = {
      if (acc.length == encode.length)
        acc
      else {
        val rowIndex = rowIndexInOccurance(c, occurance, last.toList) - 1
        val triplet = headCharOccurance(rowIndex)
        reconstruct(triplet._1._1, triplet._2,  triplet._1._1 +: acc)
      }
    }

    val res = reconstruct(headCharOccurance.head._1._1, headCharOccurance.head._2, List() )
    res.reverse.mkString("")
  }

  def compress(in: String, sc: Char) : String = {
    val res = ListBuffer[String]()
    var counter = 0
    in.foldLeft(sc){ (acc, c) =>
      if (c == acc) {
        counter += 1
        c
      } else {
        if (counter == 1)
          res.append(s"$acc")
        else
          res.append(s"$counter$acc")
        counter = 1
        c
      }
    }
    res.mkString("")
  }
}
