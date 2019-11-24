//package  com.bio3
//
//import scala.io.Source
//
//object StringTransformers {
//
//  implicit def StringToArray(s: String): Array[Array[ Int] ] =
//    s
//      .split("\n")
//      .filter(_.nonEmpty)
//      .map {
//        _
//          .replace("//", "")
//          .split(" ")
//          .filter(_.nonEmpty)
//          .map( x => x.toInt  )
//      }
//
//}
//
//object DynamicProg {
//
//  type Matrix = Array[Array[Int]]
//
//  def main(args: Array[String]) = {
//
////    val alphabet: List[Int] = List(2, 3)
////
////    def combinations(in : List[Int], acc: List[List[Int]]) : List[List[Int]] = {
////      val res = for{
////        x <- alphabet
////        n = x +: in
////        if n.sum <= 22
////      } yield n
////      if (res.isEmpty)
////        acc
////      else
////        res.map(xs => combinations(xs, res) ).flatten
////    }
////
////    val res = combinations( List(), List() )
////    println( res.toSet.toList.length )
//
//    //res.foreach(xs =>  println(xs.mkString("")))
//
//
//    //val lines = Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/BLOSUM62.txt").getLines()
//
//    //C. Longest Common Subsequence
//    val v = "ACAGATTAG"
//    val w = "T"
//
//    val score = getScore("/Users/pavel/Sources/dna-analysis/src/main/resources/BLOSUM62.txt")
//
//    val res = LCSBackTrack( v.toCharArray, w.toCharArray )
//    //val res = LCSBackTrackWithScore( v.toCharArray, w.toCharArray, score)
//    //val res = LCSBackTrack( v.toCharArray, w.toCharArray)
//
//    //res.foreach(r => println( r.mkString(" ") ) )
//
//    val output = OutputLCSWithScore(res, v.toCharArray, v.length, w.length, score)
//
//    println( output.mkString("") )
//
////    A. Change problem
////    val money = 25
////    val coins = Array(2, 3)
////    val minCoins = dpchnage(money, coins)
////    println(minCoins)
//
////    B. Manhattan tourist problem
////    import com.bio3.StringTransformers._
////
////    val n = 5
////    val m = 6
////    val down: Matrix =
////      """
////        |3 3 0 1 3 1 0
////        |3 3 4 4 3 0 4
////        |0 4 3 3 3 4 3
////        |1 2 4 4 2 3 3
////        |0 2 3 3 0 0 0
////        |""".stripMargin
////
////
////    val right: Matrix =
////      """
////        |2 3 1 0 2 1
////        |2 0 0 2 2 3
////        |2 1 1 4 4 3
////        |0 3 4 0 0 4
////        |2 1 4 3 4 3
////        |2 2 3 4 4 4
////        |""".stripMargin
////
////    println( manhattanTourist(n, m, down, right) )
//  }
//
//  def getScore(fileName: String): Array[Array[Int]] = {
//    Source.fromFile(fileName)
//      .getLines()
//      .toArray.tail.map(x => {
//      x.tail.split(" ").filter(_.nonEmpty).map(_.toInt)
//    } )
//  }
//
//  def dpchnage(money: Int, coins: Array[Int]) : Int = {
//    val minNumCoins = Array.fill(money + 1)(0)
//    (1 to money ).foreach(m => {
//      minNumCoins(m) = Int.MaxValue
//      (0 to coins.length - 1).foreach(i => {
//        if (m >= coins(i))
//          if ( minNumCoins(m - coins(i) ) + 1  < minNumCoins(m) )
//            minNumCoins(m) = minNumCoins(m - coins(i) ) + 1
//      })
//    })
//
//    minNumCoins(money )
//  }
//
//  def manhattanTourist(n: Int, m: Int, down: Matrix, right: Matrix) : Int = {
//    val s : Matrix = Array.fill(n + 1)( Array.fill(m + 1)(0) )
//    (1 to n ).foreach(i => s(i)(0) = s(i)(0) + down(i - 1)(0) )
//    (1 to m - 1).foreach(j => s(0)(j) = s(0)(j - 1) + right(0)(j - 1) )
//    (1 to n).foreach(i => {
//      (1 to m ).foreach(j =>
//        s(i)(j) = List( s(i - 1)(j) + down(i - 1)(j), s(i)(j - 1) + right(i)(j - 1) ).max
//      )
//    })
//    s(n)(m)
//  }
//
//  def LCSBackTrack(v: Array[Char], w: Array[Char]): Array[Array[Char]] = {
//    val s = Array.fill(v.length + 1)( Array.fill(w.length + 1)(0) )
//    val backtrack = Array.fill(v.length + 1)( Array.fill(w.length + 1)(' ') )
//    (1 to v.length ).foreach(i => {
//      (1 to w.length ).foreach(j => {
//        var m = 0
//        if ( v(i-1) == w(j-1) )
//          m = 1
//        s(i)(j) = List( s(i - 1)(j), s(i)(j - 1), s(i - 1)(j - 1) + m).max
//        if ( s(i)(j) == s(i - 1)(j) )
//          backtrack(i)(j) = '↓'
//        else if ( s(i)(j) == s(i)(j - 1) )
//          backtrack(i)(j) = '→'
//        else if ( s(i)(j) == s(i - 1)(j - 1) + m)
//          backtrack(i)(j) = '↘'
//      })
//    })
//    backtrack
//  }
//
//  def LCSBackTrackWithScore(v: Array[Char], w: Array[Char], score: Array[Array[Int]]): Array[Array[Char]] = {
//    val s = Array.fill(v.length + 1)( Array.fill(w.length + 1)(0) )
//    val backtrack = Array.fill(v.length + 1)( Array.fill(w.length + 1)(' ') )
//    (1 to v.length ).foreach(i => {
//      (1 to w.length ).foreach(j => {
//        s(i)(j) = List(0, s(i - 1)(j) - 5, s(i)(j - 1) - 5, s(i - 1)(j - 1) + score(i)(j) ).max
//        if ( s(i)(j) == s(i - 1)(j) - 5 )
//          backtrack(i)(j) = '↓'
//        else if ( s(i)(j) == s(i)(j - 1) - 5 )
//          backtrack(i)(j) = '→'
//        else if ( s(i)(j) == s(i - 1)(j - 1) )
//          backtrack(i)(j) = '↘'
//      })
//    })
//    backtrack
//  }
//
//  def OutputLCS(backtrack: Array[Array[Char]], v: Array[Char], i : Int, j: Int): Array[Char] = {
//    if (i == 0 || j == 0) Array()
//    else backtrack(i)(j) match {
//      case '↓' => OutputLCS(backtrack, v, i - 1, j)
//      case '→' => OutputLCS(backtrack, v, i, j - 1)
//      case _ => OutputLCS(backtrack, v, i - 1, j - 1) :+ v(i - 1)
//    }
//  }
//
//  def OutputLCSWithScore(backtrack: Array[Array[Char]], v: Array[Char], i : Int, j: Int, score: Array[Array[Int]], sc: Int = 0): Array[Char] = {
//    if (i == 0 || j == 0) {
//      println(s"Score1: ${sc}")
//      Array()
//    } else backtrack(i)(j) match {
//      case '↓' => {
////        println(s"Score2: $sc")
//        OutputLCSWithScore(backtrack, v, i - 1, j, score, sc - 5)
//      }
//
//      case '→' => {
////        println(s"Score3: $sc")
//        OutputLCSWithScore(backtrack, v, i, j - 1, score, sc - 5)
//      }
//
//      case _ => {
////        println(s"Score4: $sc")
//        OutputLCSWithScore(backtrack, v, i - 1, j - 1, score, sc + score(i - 1)(j - 1) ) :+ v(i - 1)
//      }
//    }
//  }
//
//  @inline
//  def finalizeMatrix(x: Array[Array[Int]]): IndexedSeq[IndexedSeq[Int]] = x.map(_.toIndexedSeq).toIndexedSeq
//
//  case class GlobalAlignmentResult(v: String, w: String, alignmentMatrix: IndexedSeq[IndexedSeq[Int]], sigma: Int, scoringFunction: (Char,Char) => Int) {
//    def n = v.length + 1
//    def m = w.length + 1
//    def score = alignmentMatrix(n-1)(m-1) // find the location with greatest alignment score
//    lazy val backtrackMatrix = {
//      val a = alignmentMatrix
//      val s = Array.fill[Int](n, m){0} // backtrackMatrixbioinf.hiddenmessages
//      for (i <- 1 until n){s(i)(0) = DOWN} // column 0
//      for (j <- 1 until m){s(0)(j) = RIGHT} // row 0
//      for (i <- 1 until n){
//        for (j <- 1 until m){
//          if (a(i)(j) == a(i-1)(j-1) + scoringFunction(v.charAt(i-1),w.charAt(j-1))){ // match or mismatch
//            s(i)(j) = DIAG
//          } else if (a(i)(j) == a(i-1)(j) - sigma){ // deletion
//            s(i)(j) = DOWN
//          } else if (a(i)(j) == a(i)(j-1) - sigma){ // insertion
//            s(i)(j) = RIGHT
//          }
//        }
//      }
//      finalizeMatrix(s)
//    }
//    def print = {
//      val len = n + m
//      val sbv = new StringBuilder(len)
//      val sbw = new StringBuilder(len)
//
//      var i = n-1
//      var j = m-1
//      var count = 0
//      val s = backtrackMatrix
//      while (i > 0 || j > 0 || count > len) {
//        val path = s(i)(j)
//        assert(path == DIAG || path == DOWN || path == RIGHT)
//        if (path == DIAG){ // match or mismatch
//          assert(i >= 0)
//          assert(j >= 0)
//          sbv.append(v.charAt(i-1))
//          sbw.append(w.charAt(j-1))
//          i -= 1
//          j -= 1
//        } else if (path == DOWN){ // deletion
//          assert(i >= 0)
//          sbv.append(v.charAt(i-1))
//          sbw.append("-")
//          i -= 1
//        } else if (path == RIGHT){ // insertion
//          assert(j >= 0)
//          sbv.append("-")
//          sbw.append(w.charAt(j-1))
//          j -= 1
//        }
//        count += 1
//      }
//      val vAlignment = sbv.reverseContents().mkString
//      val wAlignment = sbw.reverseContents().mkString
//      val result = new StringBuilder(len * 2)
//      result.append(score)
//      result.append(System.lineSeparator())
//      result.append(vAlignment)
//      result.append(System.lineSeparator())
//      result.append(wAlignment)
//      result.mkString
//    }
//  }
//
//  def globalAlignment(v: String, w: String, sigma: Int, scoringFunction: (Char,Char) => Int): GlobalAlignmentResult = {
//    val n = v.length + 1
//    val m = w.length + 1
//    val s = Array.fill[Int](n, m){0} // Alignment Graph
//
//    s(0)(0) = 0 // origin
//    for (i <- 1 until n){ // left column
//      val j = 0
//      s(i)(j) = s(i-1)(j) - sigma
//    }
//    for (j <- 1 until m){ // top row
//      val i = 0
//      s(i)(j) = s(i)(j - 1) - sigma
//    }
//
//    for (i <- 1 until n) {
//      for (j <- 1 until m) {
//        val mu = scoringFunction(v.charAt(i-1), w.charAt(j-1))
//        s(i)(j) = IndexedSeq[Int](
//          s(i-1)(j) - sigma, // deletion penalty
//          s(i)(j-1) - sigma, // insertion penalty
//          s(i-1)(j-1) + mu // match or mismatch value from BLOSUM62 scoring matrix
//        ).max
//      }
//    }
//    val alignmentMatrix = finalizeMatrix(s)
//    GlobalAlignmentResult(v, w, alignmentMatrix, sigma, scoringFunction)
//  }
//
//}
