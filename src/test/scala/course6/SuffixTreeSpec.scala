//package course6
//
//import org.scalatest.Matchers.contain
//import org.scalatest._
//
//class SuffixTreeSpec extends FlatSpec with Matchers {
//  import SuffixTree._
//
//  "return constructed trie" should  " be "  in {
//    val text = "ATAAATG$"
//    val actual = slideText(text)
//      .sortWith(_ > _)
//
//    val exptectedFormatedResult =
//      """
//        AAATG$
//        |G$
//        |T
//        |ATG$
//        |TG$
//        |A
//        |A
//        |AAATG$
//        |G$
//        |T
//        |G$
//        |$""".stripMargin
//        .split("\n")
//        .filter(r => r.nonEmpty )
//        .map(_.trim)
//        .toList
//
//     //exptectedFormatedResult should contain theSameElementsAs actual
//
//  }
//}
