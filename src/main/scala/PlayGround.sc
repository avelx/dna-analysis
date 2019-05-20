val peptide = "FPADFNKYVKL"

val cyclicPeptides = for{
  p <- (1 to peptide.length)
  a = peptide.takeRight(peptide.length - p)
  b = peptide.take(p)
} yield s"$a$b"

cyclicPeptides.toList