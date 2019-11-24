val input = "(+1 -2 -3 +4)"

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

val chromosome = input
    .tail.init
  .split(" ")
  .map(_.toInt)
  .toList

val inputA = "(1 2 4 3 6 5 7 8)"
val cycle = inputA
  .tail.init
  .split(" ")
  .map(_.toInt)
  .toList

def cycleToChromosome(nodes: List[Int]): List[Int] = {
  val chromosome = Array.fill( (nodes.length / 2) + 2)(0)
  (1 to (nodes.length / 2) )
    .map(j => {
      if (nodes(2 * j - 1) < nodes(2 * j  ) )
        chromosome(j - 1) = 1 //nodes(2 * j ) / 2
      else
        chromosome(j - 1) = 2 //nodes(2 * j - 1) / 2
    })
  chromosome.toList
}

val result = cycleToChromosome(cycle)
println(result.mkString(" ") )


//val result = chromosomeToCycle(chromosome)
//println(result.mkString(" ") )
