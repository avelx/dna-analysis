import com.binf.Fun._

val genome : String = "TAAAGACTGCCGAGAGGCCAACACGAGTGCTAGAACGAGGGGCGTAAACGCGGGTCCGAT"
genome.length
val r = skew(genome)

val t1 = r.zipWithIndex
val candidateIndex = t1.filter(_._1 < 0)

candidateIndex.filter(c => t1(c._2 -1)._1 > c._1 && t1(c._2 + 1)._1 > c._1)