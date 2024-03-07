import java.nio.file.Paths
import java.io.IOException
import java.nio.file.FileVisitResult
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.SimpleFileVisitor
import java.nio.file.Path
import java.nio.file.Files

def deleteDirectory(path: Path): Unit = {
  // Recursive deletion of files in the directory
  Files.walkFileTree(
    path,
    new SimpleFileVisitor[Path] {
      override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
        Files.delete(file)
        FileVisitResult.CONTINUE
      }

      override def postVisitDirectory(dir: Path, e: IOException): FileVisitResult = {
        if (e != null) throw e
        Files.delete(dir)
        FileVisitResult.CONTINUE
      }
    }
  )
}

val basedir = "nkpl/misc/combinatorial"

// deleteDirectory(Paths.get("benchmarks/combinatorial"))
// Files.createDirectory(Paths.get("benchmarks/combinatorial"))

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
  val pw = new PrintWriter(new File(s"$basedir/flip$n.nkpl"))
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

  // Write to file $basedir/inc{n}.nkpl
  import java.io._
  val pw = new PrintWriter(new File(s"$basedir/inc$n.nkpl"))
  pw.write(sb.toString)
  pw.close
}

// Nondeterministic (@sw←0 ∪ @sw←1 ∪ ... ∪ @sw←n) ⋅ (@pt←0 ∪ @pt←1 ∪ ... v @pt←n) ⋅ (@dst←0 ∪ @dst←1 ∪ ... ∪ @dst←n)

def genNondet(v: String, n: Int) =
  s"""${(0 to n).map(i => s"@$v←$i").mkString(" ∪ ")}"""

val vars = List("sw", "pt", "dst")
for n <- 1 to nmax do {
  val sb = new StringBuilder

  for v <- vars do {
    sb.append(s"${v}E = ${genNondet(v, n)}\n")
  }

  sb.append(s"all = ${vars.map(v => s"${v}E").mkString(" ⋅ ")}\n")
  sb.append(s"check all ⋅ all ≡ all\n")

  // Write to file $basedir/nondet{n}.nkpl
  import java.io._
  val pw = new PrintWriter(new File(s"$basedir/nondet$n-${vars.mkString("")}.nkpl"))
  pw.write(sb.toString)
  pw.close
}

// FalseTrue (@x1=0 ⋅ @x1←1 + ... + @xk=0 ⋅ @xk←1)⋆

def genFalseTrue(v: String, n: Int) =
  s"""(${(1 to n).map(i => s"@$v$i=0⋅@$v$i←1").mkString(" + ")})⋆"""

// (@x1=0 ⋅ @x1←1 + ε)⋅  ... ⋅ (@xk=0 ⋅ @xk←1 + ε)
def genFalseTrue2(v: String, n: Int) =
  s"""${(1 to n).map(i => s"(@$v$i=0⋅@$v$i←1 + ε)").mkString(" ⋅ ")}"""

genFalseTrue("x", 3)
genFalseTrue2("x", 3)

// Generate equivalence tests

for n <- 1 to nmax do {
  val sb = new StringBuilder
  sb.append(s"check ${genFalseTrue("x", n)} ≡ ${genFalseTrue2("x", n)}\n")

  // Write to file $basedir/falsetrue{n}.nkpl
  import java.io._
  val pw = new PrintWriter(new File(s"$basedir/falsetrue$n.nkpl"))
  pw.write(sb.toString)
  pw.close
}

// FlipAll ((@x1=0 ⋅ @x1←1 + @x1=1 ⋅ @x1←0) + ... + (@xk=0 ⋅ @xk←1 + @xk=1 ⋅ @xk←0))⋆ ≡ (@x1=0 ⋅ @x1←1 + @x1=1 ⋅ @x1←0)⋆ ⋅ ... ⋅ (@xk=0 ⋅ @xk←1 + @xk=1 ⋅ @xk←0)⋆

// @x1=0 ⋅ @x1←1 + @x1=1 ⋅ @x1←0
def genFlip2(n: Int) =
  s"""flip_x$n = @x$n=0 ⋅ @x$n←1 + @x$n=1 ⋅ @x$n←0"""

def genFlipAll(n: Int) =
  s"""(${(1 to n).map(i => s"flip_x$i").mkString(" + ")})⋆"""

def genFlipAll2(n: Int) =
  s"""${(1 to n).map(i => s"(flip_x$i)⋆").mkString("⋅")}"""

for n <- 1 to nmax do {
  val sb = new StringBuilder
  for (i <- 1 to n) {
    sb.append(genFlip2(i))
    sb.append("\n")
  }
  sb.append(s"check ${genFlipAll(n)} ≡ ${genFlipAll2(n)}\n")

  // Write to file $basedir/flipall{n}.nkpl
  import java.io._
  val pw = new PrintWriter(new File(s"$basedir/flipall$n.nkpl"))
  pw.write(sb.toString)
  pw.close
}
