import com.dna.assembly.AssemblyFun.Graph

val matrix : Graph =
  """0 -> 3
    |     1 -> 0
    |     2 -> 1,6
    |     3 -> 2
    |     4 -> 2
    |     5 -> 4
    |     6 -> 5,8
    |     7 -> 9
    |     8 -> 7
    |     9 -> 6
  """
    .replaceAll("-", "")
    .split("\n")
    .map(_.split("//"))
    .map(_.head)
    .map(_.split(Array(',', '>')))
    .map(_.map(_.trim.toInt))

//matrix.foreach(x => println( x.mkString("") ) )