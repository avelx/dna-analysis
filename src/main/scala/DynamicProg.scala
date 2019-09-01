package  com.bio3

import scala.util.Try

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
    val money = 19322
    val coins = Array(19,14,8,5,3,1)
    val minCoins = dpchnage(money, coins)
    println(minCoins)
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

    def max(x : Int, y : Int) : Int = if (x > y) x else y

    val s : Matrix = Array.fill(n + 1)( Array.fill(m + 1)(0) )

    (1 to n ).foreach(i => s(i)(0) = s(i)(0) + down(i - 1)(0) )

    (1 to m - 1).foreach(j => s(0)(j) = s(0)(j - 1) + right(0)(j - 1) )

    (1 to n).foreach(i => {
      (1 to m ).foreach(j =>
        s(i)(j) = max( s(i - 1)(j) + down(i - 1)(j), s(i)(j - 1) + right(i)(j - 1) )
      )
    })

    s.foreach( r => println( r.mkString(" ") ) )

    s(n)(m)
  }


}
