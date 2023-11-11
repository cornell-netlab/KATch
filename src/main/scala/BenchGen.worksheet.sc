// Flip benchmarks

// flip_x1 = (@x1 = 0 ⋅ @x1 ← 1) ∪ (@x1 = 1 ⋅ @x1 ← 0) ∪ (@x1 ≠ 0 ⋅ @x1 ≠ 1 ⋅ ε)

def genFlip(n: Int): String =
  s"""flip_x$n = (@x$n=0 ⋅ @x$n←1) ∪ (@x$n=1 ⋅ @x$n←0) ∪ (@x$n≠0 ⋅ @x$n≠1 ⋅ ε)"""

val nmax = 100

for n <- 1 to nmax do {
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

// Binary number increment benchmarks

// inc_x1 = (@carry=0 ⋅ @x1=0 ⋅ @x1←1 ⋅ @carry←0) ∪ (@carry=0 ⋅ @x1=1 ⋅ @x1←0 ⋅ @carry←1) ∪ (@carry = 1 ⋅ @x1=0 ⋅ @x1←0 ⋅ @carry←1) ∪ (@carry = 1 ⋅ @x1=1 ⋅ @x1←1 ⋅ @carry←1)

def genInc(n: Int): String =
  s"""inc_x$n = (@carry=0 ⋅ @x$n=0 ⋅ @x$n←1 ⋅ @carry←0) ∪ (@carry=0 ⋅ @x$n=1 ⋅ @x$n←0 ⋅ @carry←1) ∪ (@carry = 1 ⋅ @x$n=0 ⋅ @x$n←0 ⋅ @carry←1) ∪ (@carry = 1 ⋅ @x$n=1 ⋅ @x$n←1 ⋅ @carry←1)"""

for n <- 1 to nmax do {
  val sb = new StringBuilder

  for (i <- 1 to n) {
    sb.append(genInc(i))
    sb.append("\n")
  }

  // inc = inc_x1 ⋅ inc_x2 ⋅ ... ⋅ inc_xn ⋅ @carry←0
  val inc = (1 to n).map(i => s"inc_x$i").mkString(" ⋅ ")
  sb.append(s"inc = $inc ⋅ @carry←0\n")

  // zero = @x1=0 ⋅ @x2=0 ⋅ ... ⋅ @xn=0
  val zero = (1 to n).map(i => s"@x$i=0").mkString(" ⋅ ")
  sb.append(s"\nzero = $zero\n")

  // max = @x1=1 ⋅ @x2=1 ⋅ ... ⋅ @xn=1
  val max = (1 to n).map(i => s"@x$i=1").mkString(" ⋅ ")
  sb.append(s"max = $max\n\n")

  // check zero ⋅ (inc)* ⋅ ≢  ∅
  sb.append(s"check zero ⋅ (inc)⋆ ⋅ max ≢  ∅\n")

  // Write to file benchmarks/combinatorial/inc{n}.nkpl
  import java.io._
  val pw = new PrintWriter(new File(s"benchmarks/combinatorial/inc$n.nkpl"))
  pw.write(sb.toString)
  pw.close
}
