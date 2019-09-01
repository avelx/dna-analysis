package  com.bio3

object StringTransformers {

  implicit def StringToArray(s: String): Array[Array[ Int] ] =
    s
      .split("\n")
      .filter(_.nonEmpty)
      .map {
        _
          .replace("//", "")
          .split(" ")
          .filter(_.nonEmpty)
          .map( x => x.toInt  )
      }

}

object DynamicProg {

  type Matrix = Array[Array[Int]]

  def main(args: Array[String]) = {
//    C. Longest Common Subsequence
//    val v = "TGCCCAATTCGGGTAGGCTGCGATTCCTTTGGCGAATCTAGAAACCCAAAAACACACTGGGAATCCGTGGGCTTCAAGTGGCCACACTCGGCAACAGGTCTAATTCGCCTGCGTGTTCTATCTATAAGGGGTAGGTCCATGCCTGGCTCGGTTTTTTTTTGACGAGCTTGCCAGATATCCCGGCAATTACAGTTACTCTATCGAATTTATAAGTGGTCCGGTCTGAGGGATGTCACTACGAGTAGTTCGGCCGAAACGTGACTTGCAGCCGAAAAGGCCCTCAGCTGTCCTACACAAGCAGGCTGCATCAATCACGCGAGTGTTACCCGCCGTCGGCAGATTGTGTCAGTGTGCTAATCCCCCCCCCCCGTGCAAGGCAGTCAATTCTCACATCGCCTAATGCTGATTGCAAATCGCAGCGGGGCCCAGATTCACTCGTTGCGGAGTCTCGCTTCCCTAGATCCAGACTTACATGAATGACTAAGGGCTATTAGAATAGAGAACAACCCAATCTGGAGCCTTTTTATCGGGATGCGCTTCGCGTCTACCCATTGAACACAGTATATGCACGGTAAGCCACCCGCGGACGGATCATATCACTCAAAGGCGGTAAAATCTGTGTCGGCTTAGTTGTGACAACTGCCACCACTCGGCCGCCCGCCGTATGTGGGCGGCCATAACCTGGTCATATCCGGGAGTTAATGTGAATCGTATCGCGAATTCACAACTTGATCCTCATGAGAGGCGTCGCCAAGGTCTTTAGACTAACACTATGCGCGTACACTCCACGACATTAGATTATTGTTTGTACGACAGGAGCGTTTTACAGAGGTCTCTGCGCGATACAGGACCGGTGGTCGTAGGAAACTAGGCACAAAATCCCCGGTGCTTATCGGAGTGACTTCAGTCTAGTCTAGTGGCGTTCTCTCGAATAACCGTCGGATGTGCACGAGG"
//    val w = "AGGTGGTGCTAGTGATACCTAGGTTATTCGAACTACTCGCAGAAACCCATGATGAGGTCGTCGCCGCTGCCCCGGGAAGAGACTGGCTTGCTAACAACAACAGGAGAGAACGAGTCGTTTCCCGGGCCAGGTTATCCGCCCGCTCGAGGAGGCGGGACGAAGGGGCTCGCACTCGTACGAAAAGAATCCTGAGGGCTGGCTCAAATAAGAATATGTAGTAAAACGGATAGGGGGGAGGAAAGAAGATCCACGATATAGCTGCCAACCTTACAACTGCGCCCGAATCCACGGCTAAGTCGATGGGTTGCCGGGAATGATTTGTAACTGGCATATTCAATGGATCGTGATTGTCGCTTAATCAAAATTCGCTGAGGTTACGTCAATCTACTCTAAACAAGAGAGCAATACTATGCTTACTACGTAGTGCAACGGATTATGGCGCTGAACTAGATGGACCTGGTCGTGATAGTTCCGGATTGGCTCGGCAAAACTCATTCCGGCGTTGTGAGTTACTAGTGATGAGGCAAACCGGCACCTAGAATAACGGGATGGGACCACGCGTGATCTATTAATACCGACCGTGTTCATGTATTTCGAACGTATTTGGCGACTCTCCAACCGACCACGGCATAAGCCTCACATAGTTGGATAGCGTATTAGGTAGAACTCAACTTTGAGTGCTTTCGGAGGGTAATAGAGTACGCTTTTACTGGCTTTCTTTCCGCGTTAAGCGGCTGGGTCGACTTGGGGATTCGCTAAGGCCCAGGTGAGTCAGTAATTTAGCACAGACAAATATAGTTATATTAGATAAGCTCCGTTGGCGAACCATGCACCGTGAGCAGAGTAATAGGTGTGCCCGCGATCTGCGCGTCTCCGGGGCACCACATAGGCTCATCCTAACACAACAAAGAGTTCACGAATCTGACGATCCATGACCCCTG"
//
//    val res = LCSBackTrack( v.toCharArray, w.toCharArray)
//    //res.foreach(r => println( r.mkString(" ") ) )
//
//    val output = OutputLCS(res, v.toCharArray, v.length, w.length)
//
//    println( output.mkString("") )

//    A. Change problem
//    val money = 19322
//    val coins = Array(19,14,8,5,3,1)
//    val minCoins = dpchnage(money, coins)
//    println(minCoins)

//    B. Manhattan tourist problem
//    import com.bio3.StringTransformers._
//
//    val n = 5
//    val m = 6
//    val down: Matrix =
//      """
//        |3 3 0 1 3 1 0
//        |3 3 4 4 3 0 4
//        |0 4 3 3 3 4 3
//        |1 2 4 4 2 3 3
//        |0 2 3 3 0 0 0
//        |""".stripMargin
//
//
//    val right: Matrix =
//      """
//        |2 3 1 0 2 1
//        |2 0 0 2 2 3
//        |2 1 1 4 4 3
//        |0 3 4 0 0 4
//        |2 1 4 3 4 3
//        |2 2 3 4 4 4
//        |""".stripMargin
//
//    println( manhattanTourist(n, m, down, right) )
  }

  def dpchnage(money: Int, coins: Array[Int]) : Int = {
    val minNumCoins = Array.fill(money + 1)(0)
    (1 to money ).foreach(m => {
      minNumCoins(m) = Int.MaxValue
      (0 to coins.length - 1).foreach(i => {
        if (m >= coins(i))
          if ( minNumCoins(m - coins(i) ) + 1  < minNumCoins(m) )
            minNumCoins(m) = minNumCoins(m - coins(i) ) + 1
      })
    })

    minNumCoins(money )
  }

  def manhattanTourist(n: Int, m: Int, down: Matrix, right: Matrix) : Int = {
    val s : Matrix = Array.fill(n + 1)( Array.fill(m + 1)(0) )
    (1 to n ).foreach(i => s(i)(0) = s(i)(0) + down(i - 1)(0) )
    (1 to m - 1).foreach(j => s(0)(j) = s(0)(j - 1) + right(0)(j - 1) )
    (1 to n).foreach(i => {
      (1 to m ).foreach(j =>
        s(i)(j) = List( s(i - 1)(j) + down(i - 1)(j), s(i)(j - 1) + right(i)(j - 1) ).max
      )
    })
    s(n)(m)
  }

  def LCSBackTrack(v: Array[Char], w: Array[Char]): Array[Array[Char]] = {
    val s = Array.fill(v.length + 1)( Array.fill(w.length + 1)(0) )
    val backtrack = Array.fill(v.length + 1)( Array.fill(w.length + 1)(' ') )
    (1 to v.length ).foreach(i => {
      (1 to w.length ).foreach(j => {
        var m = 0
        if ( v(i-1) == w(j-1) )
          m = 1
        s(i)(j) = List( s(i - 1)(j), s(i)(j - 1), s(i - 1)(j - 1) + m).max
        if ( s(i)(j) == s(i - 1)(j) )
          backtrack(i)(j) = '↓'
        else if ( s(i)(j) == s(i)(j - 1) )
          backtrack(i)(j) = '→'
        else if ( s(i)(j) == s(i - 1)(j - 1) + m)
          backtrack(i)(j) = '↘'
      })
    })
    backtrack
  }

  def OutputLCS(backtrack: Array[Array[Char]], v: Array[Char], i : Int, j: Int): Array[Char] = {
    if (i == 0 || j == 0) Array()
    else backtrack(i)(j) match {
      case '↓' => OutputLCS(backtrack, v, i - 1, j)
      case '→' => OutputLCS(backtrack, v, i, j - 1)
      case _ => OutputLCS(backtrack, v, i - 1, j - 1) :+ v(i - 1)
    }
  }

}
