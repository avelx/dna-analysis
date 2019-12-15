import course4.EvolutionaryTree.{Edge, Matrix}

val lines = scala.io.Source.fromFile("/Users/pavel/Sources/dna-analysis/src/main/resources/additivePolygeny.txt").getLines()

type Row = Array[Int]
type Edges = Array[Edge]
type Matrix = Array[Row]
type Strings = Array[String]
type Tree = Edges

val matrix = lines.toArray
  .map(_.split("\t"))
  .map(_.map(_.toInt).toArray[Int])

