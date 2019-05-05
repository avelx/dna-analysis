import com.dna.assembly.AssemblyFun.stringReconstruction

val k = 4

def format(k: Int, s: String) : String = "0" * (k - s.length) + s

val kmers = (0 to Math.pow(2.toDouble, k.toDouble).toInt )
  .map(_.toBinaryString)
  .map( format(k, _) )

val text = stringReconstruction(kmers.toSeq)
println(text)