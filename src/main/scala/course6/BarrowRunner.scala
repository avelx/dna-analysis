package course6

object BarrowRunner extends  App {

  import  BurrowWheelerTransform._

  //val in2 = "panamabananas$"
  val in2 = "abracadabra$"

//  println( shiftLeft (in2) )
//  println( shiftRight(in2) )
//  sys.exit(0)

  val in2Enc = transform(in2)

  println()
  println(in2Enc)
  println(in2Enc)
  println()

//  println{
    transformBack(in2Enc)
//  }

}
