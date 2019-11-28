package com.avel.dna

import org.scalatest._

class EvolutionaryTreeSpec extends FlatSpec {

 it should "return next result" in {

  val input =
   """
     |32
     |60->61:5
     |60->3:9
     |60->59:7
     |61->60:5
     |61->19:7
     |61->42:13
     |19->61:7
     |42->61:13
     |42->20:10
     |42->41:10
     |3->60:9
     |59->60:7
     |59->8:7
     |59->58:8
     |8->59:7
     |58->59:8
     |58->25:10
     |58->57:13
     |25->58:10
     |57->58:13
     |57->31:12
     |57->56:11
     |31->57:12
     |56->57:11
     |56->30:14
     |56->55:8
     |30->56:14
     |55->56:8
     |55->5:7
     |55->54:5
     |5->55:7
     |54->55:5
     |54->21:15
     |54->53:10
     |21->54:15
     |53->54:10
     |53->27:14
     |53->52:6
     |27->53:14
     |52->53:6
     |52->10:10
     |52->51:12
     |10->52:10
     |51->52:12
     |51->28:5
     |51->50:11
     |28->51:5
     |50->51:11
     |50->23:10
     |50->49:13
     |23->50:10
     |49->50:13
     |49->9:14
     |49->48:10
     |9->49:14
     |48->49:10
     |48->17:7
     |48->47:10
     |17->48:7
     |47->48:10
     |47->18:10
     |47->46:8
     |18->47:10
     |46->47:8
     |46->12:10
     |46->45:8
     |12->46:10
     |45->46:8
     |45->6:6
     |45->44:10
     |6->45:6
     |44->45:10
     |44->14:8
     |44->43:6
     |14->44:8
     |43->44:6
     |43->0:11
     |43->29:7
     |0->43:11
     |29->43:7
     |20->42:10
     |41->42:10
     |41->13:9
     |41->40:10
     |13->41:9
     |40->41:10
     |40->2:8
     |40->39:13
     |2->40:8
     |39->40:13
     |39->16:10
     |39->38:11
     |16->39:10
     |38->39:11
     |38->1:12
     |38->37:6
     |1->38:12
     |37->38:6
     |37->7:14
     |37->36:8
     |7->37:14
     |36->37:8
     |36->22:5
     |36->35:10
     |22->36:5
     |35->36:10
     |35->15:8
     |35->34:15
     |15->35:8
     |34->35:15
     |34->11:9
     |34->33:8
     |11->34:9
     |33->34:8
     |33->4:9
     |33->32:9
     |4->33:9
     |32->33:9
     |32->24:9
     |32->26:11
     |24->32:9
     |26->32:11
     |""".stripMargin
//   """
//     |4
//     |0->4:11
//     |1->4:2
//     |2->5:6
//     |3->5:7
//     |4->0:11
//     |4->1:2
//     |4->5:4
//     |5->4:4
//     |5->3:7
//     |5->2:6
//     |""".stripMargin

  final case class Edge(from: Int, to: Int, weight: Int)
  val arr = input
    .split("\n")
    .map(_.trim)
    .filter(_.nonEmpty)
    .map(_.split("//").head.trim )

  val n = arr.head.toInt
  // Get list of edges
  val edges : Seq[Edge]  = for {
   a <- arr.tail
   xs = a.split("->")
   ws = xs.tail.head.split(":")
  } yield Edge( xs.head.toInt, ws.head.toInt, ws.tail.head.toInt )

  // Number of Vertices in the Graph
  val v : Int  = 62 //edges.map(_.from).max + 1
  val dist = Array.fill(v)( Array.fill(v)(Int.MaxValue) )
  edges.foreach( e =>  dist(e.from)(e.to) = e.weight)
  (0 to v - 1).foreach(j => dist(j)(j) = 0)

  for {
   k <- 0 to v - 1
   i <- 0 to v - 1
   j <- 0 to v - 1
   if dist(i)(k) != Int.MaxValue && dist(k)(j) != Int.MaxValue
   if dist(i)(j) > dist(i)(k) + dist(k)(j)
  } yield dist(i)(j) = dist(i)(k) + dist(k)(j)

  val realDistance = dist.map(r => r.map(x => if (x == Int.MaxValue) 0 else x) )
  val output = Array.fill(n)( Array.fill(n)(0) )
  for {
   i <- 0 to n - 1
   j <- 0 to n - 1
  } yield output(i)(j) = realDistance(i)(j)

  output.foreach(row => println( row.mkString(" ")))

 }

}