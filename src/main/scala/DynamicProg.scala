package  com.bio3

object DynamicProg {

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



}
