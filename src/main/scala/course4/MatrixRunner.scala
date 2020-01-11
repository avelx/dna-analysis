package course4

object MatrixRunner extends  App {
  println("Data test ")

  type Matrix = Array[Array[Float]]

  val A : Array[Array[Float]] = Array(
    Array(0, 20, 9, 11),
    Array(20, 0, 17, 11),
    Array(9, 17, 0, 8),
    Array(11, 11, 8, 0)
//    Array(0, 17, 21, 31, 23),
//    Array(17, 0, 30, 34, 21),
//    Array(21, 30, 0, 28, 39),
//    Array(31, 34, 28, 0, 43),
//    Array(23, 21, 39, 43, 0)
  )

  class MatrixExtension(matrix: Matrix) {


    private val size = matrix(0).length

    def |-|(index: Int) : Matrix = {
      for {
        row <- matrix
      } yield row.take(index) ++ row.drop(index + 1)
    }

    def -=(index: Int) : Matrix =
      matrix.zipWithIndex
        .filter( p => p._2 != index )
        .map(_._1)

    //  case class P( element: Float, index: Int, foundIndex: Int )
    case class P( element: Float, coordinats: Option[(Int, Int)])


    def argmin : (Int, Int) = {
      //val p = matrix.flatten.foldLeft( P(Int.MaxValue,0, 0) )( (curr, e) =>
      //  if (e != 0.0F && curr.element > e) P(e, curr.index + 1, curr.index)
      //  else  P(curr.element, curr.index + 1, curr.foundIndex) )
      //p.foundIndex
      {
        for {
          x <- 0 to size - 1
          y <- 0 to size - 1
          if x < y
        } yield (x, y)
        }.foldLeft( P(Float.MaxValue, None) )(
        (curr, cord) =>
          if ( curr.element > matrix(cord._1)(cord._2).asInstanceOf[Float] )
            P( matrix(cord._1)(cord._2).asInstanceOf[Float], Some(cord) )
          else
            curr
      ).coordinats.get
    }

    def printm =
      matrix.foreach(row => println( row.mkString("\t\t") ))

    def avg(fromCol: Int, rowIndex: Int ) : Array[Float] = {
      matrix(rowIndex).drop(fromCol)
        .zip( matrix(rowIndex + 1).drop(fromCol) )
        .map( p => (p._1 + p._2) / 2 )
    }

    def replace(fromCol: Int, rowIndex: Int ): Matrix = {
      val c = avg(fromCol, rowIndex)
      for {
        i <- fromCol to size - 1
      } yield {
        matrix(rowIndex + 1)(i) = c(i - fromCol)
        matrix(i)(rowIndex + 1) = c(i - fromCol)
      }
      matrix
    }

    def merge(baseIndex: Int = 0, withIndex: Int = 0, level: Int = 0) : Matrix = {
      if (size != 3) {
        val newDist = matrix(baseIndex).map(e => e * level)
          .zip(matrix(withIndex))
          .map(p => (p._1 + p._2) / (level + 1))

        for {
          i <- 1 to size - 1
        } yield {
          matrix(baseIndex)(i) = newDist(i)
          matrix(i)(baseIndex) = newDist(i)
        }
        matrix
      } else {
        val x = matrix(0).drop(1).sum / 2
        Array(
          Array(0F, x),
          Array(x, 0F)
        )
      }
    }

  }

  implicit def matrixToMatrixExtension(m: Matrix) = new MatrixExtension(m)

  var level = 1
  A.printm
  println()

  val am = A argmin
  val B = (( A merge(am._1, am._2, level) ) |-| am._2 ) -= am._2
  B.printm
  println()


  level += 1
  val am2 = B argmin
  val C = ( (B merge (am2._1, am2._2, level) ) |-| am2._2 ) -= am2._2
  C.printm

//  val am3 = C argmin
//  val D = ( ( C merge() ) |-| am3._2 ) -= am3._2
//  D.printm
}
