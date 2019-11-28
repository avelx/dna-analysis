val input =
  """
    |4
    |0->4:11
    |1->4:2
    |2->5:6
    |3->5:7
    |4->0:11
    |4->1:2
    |4->5:4
    |5->4:4
    |5->3:7
    |5->2:6
    |""".stripMargin


val arr = input
  .split("\n")
  .map(_.trim)
  .filter(_.nonEmpty)
  .map(_.split("//").head.trim )
  .tail

val n = arr.head.toInt


