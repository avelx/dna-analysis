package course6

import org.scalatest._

class DnaMutationSpec extends FlatSpec {

  "return constructed trie" should  " be "  in {
    val input = List("ATAGA","ATC","GAT")
    val actual = DnaMutation.createTrie(input)


    val exptectedFormatedResult =
      """
        |0->1:A
        |1->2:T
        |2->3:A
        |3->4:G
        |4->5:A
        |2->6:C
        |0->7:G
        |7->8:A
        |8->9:T""".stripMargin
  }

}
