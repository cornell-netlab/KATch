// flip_x1 = (@x1 = 0 ⋅ @x1 ← 1) + (@x1 = 1 ⋅ @x1 ← 0) + (@x1 ≠ 0 ⋅ @x1 ≠ 1 ⋅ ε)

def genFlip(n: Int): String =
  s"""flip_x$n = (@x$n=0 ⋅ @x$n←1) + (@x$n=1 ⋅ @x$n←0) + (@x$n≠0 ⋅ @x$n≠1 ⋅ ε)"""

for n <- 1 to 20 do {
  val sb = new StringBuilder

  for (i <- 1 to n) {
    sb.append(genFlip(i))
    sb.append("\n")
  }

  // flip = flip_x1 ⋅ flip_x_2 ⋅ ... ⋅ flip_xn
  val flip = (1 to n).map(i => s"flip_x$i").mkString(" ⋅ ")
  sb.append(s"\nflip = $flip\n\n")
  sb.append("check flip ⋅ flip ≡ ε")

  // Write to file benchmarks/combinatorial/flip{n}.nkpl
  import java.io._
  val pw = new PrintWriter(new File(s"benchmarks/combinatorial/flip$n.nkpl"))
  pw.write(sb.toString)
  pw.close
}
