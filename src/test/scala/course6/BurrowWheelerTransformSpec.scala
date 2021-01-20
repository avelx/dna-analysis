package course6

import org.scalatest.FlatSpec

class BurrowWheelerTransformSpec extends FlatSpec {
    import BurrowWheelerTransform._
    "transform for GCGTGCCTGGTCA$" should  " return ACTGGCT$TGCGGC"  in {
      val in = "GCGTGCCTGGTCA$"
      val actual = transform(in)
     assert(actual == "ACTGGCT$TGCGGC")
    }

}
