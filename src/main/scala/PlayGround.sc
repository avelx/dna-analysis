


def dnaTranscription(dna: String) : String = dna.map(c => c match {
  case 'T' => 'U'
  case c: Char => c
})

dnaTranscription("TATACGAAA")