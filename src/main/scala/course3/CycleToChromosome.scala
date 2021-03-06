import scala.collection.mutable.ListBuffer

object CycleToChromosome {

  def main(args: Array[String]) : Unit = {
    //val inputA = "(1 2 4 3 6 5 7 8)"
    val inputA = "(2 1 4 3 6 5 8 7 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 23 26 25 27 28 29 30 31 32 33 34 35 36 37 38 39 40 42 41 44 43 46 45 48 47 49 50 51 52 53 54 56 55 58 57 59 60 62 61 63 64 65 66 67 68 70 69 72 71 74 73 75 76 77 78 80 79 82 81 84 83 86 85 88 87 89 90 92 91 94 93 95 96 98 97 100 99 102 101 103 104 106 105 107 108 110 109 111 112 113 114 116 115 117 118 120 119 121 122 123 124 126 125 127 128 129 130 132 131 133 134)"
    val cycle = inputA
      .tail.init
      .split(" ")
      .map(_.toInt)
      .toList


    def chromosomeToCycle(chromosome: List[Int]) : List[Int] = {
      val nodes = Array.fill(chromosome.length * 2  + 2)(0)
      (1 to chromosome.length )
        .map(j => {
          val i = chromosome(j - 1)
          if (i > 0){
            nodes(2 * j) = 2 * i
            nodes(2 * j - 1) = 2 * i - 1
          } else {
            nodes(2 * j - 1) = - 2 * i
            nodes(2 * j) = - 2 * i - 1
          }
        })
      nodes.toList.tail.init
    }

    def cycleToChromosome(nodes: Array[Int]): List[Int] = {
      val chromosome = Array.fill(nodes.length / 2)(0)
      val (even_, odd_) = nodes
        .zipWithIndex
        .partition( t  =>
          if (t._2 % 2 == 0 ) true
          else
            false )
      val evenNodes = even_.map(_._1)
      val oddNodes = odd_.map(_._1)

      (0 to evenNodes.length - 1).map(j => {
        if (oddNodes(j) < evenNodes(j))
          chromosome(j) = -evenNodes(j) / 2
        else
          chromosome(j) = oddNodes(j) / 2
      })

      chromosome.toList
    }

    def colorEdges(chromosomes : List[Array[Int]]): List[(Int, Int)] = {
      val edges = new ListBuffer[(Int, Int)]()

      chromosomes.map(chromosome => {
        val nodes = chromosomeToCycle(chromosome.toList)

        val (even_, odd_) = nodes
          .zipWithIndex
          .partition( t  =>
            if (t._2 % 2 == 0 ) true
            else
              false )
        val evenNodes = even_.map(_._1)
        val oddNodes = odd_.map(_._1)
        edges.appendAll(  oddNodes zip (evenNodes.tail :+ evenNodes.head) )

      })

      edges.toList
    }

    //val g = "(+1 -2 -3)(+4 +5 -6)"
    val g = "(+1 +2 +3 +4 -5 -6 +7 +8 +9 -10 +11 -12 -13 +14 -15 -16 +17 -18 +19 +20 +21 -22 -23 +24)(-25 +26 -27 -28 -29 -30 -31 -32 +33 -34 -35 +36 -37 -38 -39 -40 +41 +42 +43 -44 +45 +46 -47 +48 +49 -50 +51 +52 +53 -54)(+55 -56 +57 -58 -59 -60 -61 -62 -63 +64 +65 +66 -67 -68 +69 -70 +71 -72 -73 +74 +75 -76 +77 -78 -79)(-80 -81 -82 -83 +84 -85 +86 +87 +88 -89 -90 -91 +92 +93 -94 -95 +96 -97 -98 +99 +100 -101 -102 -103 +104 +105)(-106 +107 +108 -109 -110 +111 -112 -113 -114 +115 +116 -117 -118 -119 -120 +121 -122 -123 -124 -125 +126 -127 -128 -129 -130 +131 -132 -133 +134 +135 +136)(+137 -138 +139 -140 -141 -142 -143 -144 +145 +146 +147 +148 +149 -150 -151 -152 -153 -154 +155 -156 +157 -158 -159 -160 +161)(+162 -163 +164 +165 +166 +167 +168 +169 +170 +171 -172 +173 -174 -175 +176 -177 -178 +179 -180 -181 +182 -183 -184 -185)"
    val r : List[Array[Int]]= g
      .split(')')
        .map(_.replace("(", ""))
      .map( _.split(" ").map(_.toInt) ).toList

    //println(r.mkString("  "))

    val result = colorEdges(r)

    println(
          result
          .map(p => s"(${p._1}, ${p._2})")
            .mkString(", ")
          )

    //val result = cycleToChromosome(cycle.toArray)
    //val str = result.map(x => if (x > 0) s"+$x" else s"$x" ).mkString(" ")
    //println(s"($str)" )
  }
}
