val row = "0 71 71 99 101 103 113 113 114 128 128 131 147 163 170 172 184 199 215 227 227 231 244 259 260 266 271 286 298 298 310 312 328 330 330 372 385 391 394 399 399 399 401 413 423 426 443 443 470 493 498 502 513 519 526 527 541 554 556 557 564 569 590 598 616 626 640 654 657 658 665 670 682 697 697 703 711 729 729 753 753 771 779 785 785 800 812 817 824 825 828 842 856 866 884 892 913 918 925 926 928 941 955 956 963 969 980 984 989 1012 1039 1039 1056 1059 1069 1081 1083 1083 1083 1088 1091 1097 1110 1152 1152 1154 1170 1172 1184 1184 1196 1211 1216 1222 1223 1238 1251 1255 1255 1267 1283 1298 1310 1312 1319 1335 1351 1354 1354 1368 1369 1369 1379 1381 1383 1411 1411 1482"

row.split(" ").length

val peptide = "IAQMLFYCKVATN"

val variations = {
  for {
    i <- 1 to peptide.length - 1
    comb = (peptide + peptide.take(i - 1) ).sliding(i, 1)
  } yield comb
}.flatten

variations.length

variations.toList.sortBy(_.length).foreach(println)

/*

val r = "LEQN"


val variations = {
  for {
    i <- 1 to r.length
    comb = (r :+ r.head).sliding(i, 1)
  } yield comb
}.flatten.map(row => row.sorted).toSet

val res = variations.toList.sortBy(_.length)

res.length
 */