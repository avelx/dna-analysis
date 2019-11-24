object ScoreRunner {

  def score(v: String, w: String, m: Int, ms: Int, ind: Int): Int = {
    var score = 0
    (0 to v.length - 1).map(i => {
      if ( v(i) == w(i) ) // match
        score += m
      else if ( v(i) == '-' || w(i) == '-') // indel
        score += ind
      else // mismatch
        score += ms
    })
    score
  }

  def main(args: Array[String]): Unit = {

    //val result = score("TCGAC--ATT", "CC---GAA-T", 1, -1, -2)
    val result = score("TCGAC--ATT", "CC---GAA-T", 1, -1, -4)

    println( result )
  }
}