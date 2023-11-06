import scala.collection.immutable.SortedSet
// Memoizes a function of one argument. The memoized function returns the cached result if the same argument is passed again.
def memoize[A, B](f: A => B): A => B =
  val cache = scala.collection.mutable.Map.empty[A, B]
  (a: A) => cache.getOrElseUpdate(a, f(a))

// Memoizes functions that take more arguments.
def memoize2[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B =
  val g = memoize(f.tupled)
  (a1, a2) => g((a1, a2))
def memoize3[A1, A2, A3, B](f: (A1, A2, A3) => B): (A1, A2, A3) => B =
  val g = memoize(f.tupled)
  (a1, a2, a3) => g((a1, a2, a3))

type Var = String
type Val = Int

// A FDD first splits the packet space into a finite number of subsets,
// and then applies a set of mutations to each subset.
class FDD

// Applies mutations to each packet. Each mutation is a map from fields to a values.
// If the set of mutations is empty, the packet is dropped. If the set is a singleton, the packet is sent through the single map.
// If the set has more than one element, the packet is sent through each map, and the results are joined.
case class Mut(value: Set[Map[Var, Val]]) extends FDD {
  override val hashCode = scala.util.hashing.MurmurHash3.productHash(this)
}
object Mut {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Mut, Mut]
  def apply(value: Set[Map[Var, Val]]): Mut =
    val v = new Mut(value)
    cache.getOrElseUpdate(v, v)
}

// Case split on the field x and then apply the corresponding FDD map(v).
// For values of x not in the map, apply the default FDD.
case class Test(x: Var, map: Map[Val, FDD], default: FDD) extends FDD {
  override val hashCode = scala.util.hashing.MurmurHash3.productHash(this)
}
object Test {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Test, Test]
  def apply(x: Var, map: Map[Val, FDD], default: FDD): FDD =
    // Remove elements from the map that are the same as the default case
    val map1 = map.filter((v, fdd) => !(fdd eq default))
    // If the map is empty, return the default case
    if map1.isEmpty then default
    else
      val v = new Test(x, map, default)
      cache.getOrElseUpdate(v, v)
}

// Specializes the FDD to the given assignment/test.
// We assume that the incoming packets satisfy the assignment/test.
lazy val spec: (Var, Val, FDD) => FDD = memoize3 { (x, v, fdd) =>
  fdd match {
    case Mut(s) =>
      // Removes redundant mutations (if we test that x = v and then set x := v, we can remove the set)
      Mut(s.map(m => if m.contains(x) && m(x) == v then m - x else m))
    case Test(y, map, default) =>
      if x == y then map.getOrElse(v, default)
      else Test(y, map.map((v, fdd) => (v, spec(x, v, fdd))), spec(x, v, default))
  }
}

lazy val specDefault: (Var, FDD) => FDD = memoize2 { (x, fdd) =>
  fdd match {
    case Mut(s) => fdd
    case Test(y, map, default) =>
      if x == y then default
      else Test(y, map.map((v, fdd) => (v, specDefault(x, fdd))), specDefault(x, default))
  }
}

// Does the test and specializes the child FDDs to that test.
def TestSpec(x: Var, map: Map[Val, FDD], default: FDD): FDD =
  val map2 = map.map((v, fdd) => (v, spec(x, v, fdd)))
  Test(x, map2, default)
// This one also rearranges the tests in the correct order and removes redundant mutations and tests.
def TestMove(x: Var, map: Map[Val, FDD], default: FDD): FDD =
  // We want to figure out the maximum variable that is tested in any of the cases,
  // and then move that test over x if necessary.
  val map2 = map.map((v, fdd) => (v, spec(x, v, fdd)))
  def getVar(fdd: FDD) = fdd match {
    case Mut(_) => None
    case Test(y, _, _) => Some(y)
  }
  val vars = map.flatMap((v, fdd) => getVar(fdd)).toSet ++ getVar(default).toSet
  if vars.isEmpty || vars.min > x then return Test(x, map2, default) // no need to move anything
  val minvar = vars.min
  // We need to move minvar over x.
  val dom = map2.flatMap((v, fdd) =>
    fdd match {
      case Mut(s) => Set.empty
      case Test(y, map, default) => if y == minvar then map.keys else Set.empty
    }
  )
  // We branch minvar on the union of the domains of all the branches.
  // In each branch, we further branch on the original test using TestMove.
  // In the default case, we branch on the original test using TestMove.
  TestSpec(
    minvar,
    dom.map(v => v -> TestMove(x, map2.map((v, fdd) => v -> spec(minvar, v, fdd)), spec(minvar, v, default))).toMap,
    TestMove(x, map2.map((v, fdd) => v -> specDefault(minvar, fdd)), specDefault(minvar, default))
  )

val zero = Mut(Set()) // rejects all packets
val one = Mut(Set(Map())) // accepts all packets
def set(m: (Var, Val)) = Mut(Set(Map(m))) // sets field x to value v
def test(m: (Var, Val)) = Test(m._1, Map(m._2 -> one), zero) // accepts packets with field x set to v

// Sends all packets through both left and right, and joins the results.
lazy val plus: (FDD, FDD) => FDD = memoize2 { (left, right) =>
  (left, right) match {
    case (Mut(s1), Mut(s2)) => Mut(s1 ++ s2)
    case (Test(x, map, default), Mut(s)) => TestSpec(x, map.map((v, fdd) => (v, plus(fdd, right))), plus(default, right))
    case (Mut(s), Test(x, map, default)) => plus(right, left)
    case (Test(xL, mapL, defaultL), Test(xR, mapR, defaultR)) =>
      if xL == xR then TestSpec(xL, (mapL.keys ++ mapR.keys).map(v => v -> plus(mapL.getOrElse(v, defaultL), mapR.getOrElse(v, defaultR))).toMap, plus(defaultL, defaultR))
      else if xL < xR then TestSpec(xL, mapL.map((v, fdd) => (v, plus(fdd, right))), plus(defaultL, right))
      else plus(right, left)
  }
}

def sum(fdds: Iterable[FDD]): FDD = fdds.foldLeft(zero)(plus)

// Sends all packets through left, and then sends the resulting packets through right.
lazy val seq: (FDD, FDD) => FDD = memoize2 { (left, right) =>
  (left, right) match {
    case (Mut(s1), Mut(s2)) => Mut(s1.flatMap(a1 => s2.map(a2 => a1 ++ a2)))
    case (Mut(s), Test(x, map, default)) =>
      // Here we want to pull the test over the mutations.
      // If the mutation affects the test variable x, then it goes to the appropriate branch of the test AND removes the test
      // If the mutation does not affect x, then it goes to all branches of the test, but does not remove the test
      val s1 = s.filter(m => m.contains(x))
      val s2 = s.filter(m => !m.contains(x))
      val fdd1 = sum(map.map((v, fdd) => seq(Mut(s1.filter(m => m(x) == v)), fdd)))
      val fdd2 = seq(Mut(s1.filter(m => !map.contains(m(x)))), default)
      val fdd3 = TestSpec(x, map.map((v, fdd) => v -> seq(Mut(s2), fdd)), seq(Mut(s2), default))
      plus(fdd1, plus(fdd2, fdd3))
    case (Test(x, map, default), _) =>
      TestMove(x, map.map((v, fdd) => v -> seq(fdd, right)), seq(default, right))
  }
}

lazy val neg: FDD => FDD = memoize {
  case Mut(s) => if s.isEmpty then one else zero
  case Test(x, map, default) => TestSpec(x, map.map((v, fdd) => (v, neg(fdd))), neg(default))
}

def star(fdd: FDD): FDD =
  def fix(x: FDD): FDD =
    val y = plus(one, seq(x, fdd))
    if x eq y then y else fix(y)
  fix(zero)

// Syntactic sugar for plus and seq.
implicit class FDDOps(left: FDD) {
  def +(right: FDD): FDD = plus(left, right)
  def *(right: FDD): FDD = seq(left, right)
}

set("x" -> 1) * set("x" -> 2)
set("x" -> 1) * set("y" -> 2)
set("x" -> 2) * test("x" -> 1)
set("x" -> 1) * test("x" -> 1)
test("x" -> 1) * set("x" -> 2)
test("x" -> 1) * set("x" -> 1)
set("y" -> 2) * test("x" -> 1)
test("y" -> 2) * set("x" -> 1)

def randomMut(vars: Seq[Var], vals: Seq[Val]) =
  def randomAsgn() =
    // Select a random subset of the vars
    val vars1 = vars.filter(_ => scala.util.Random.nextBoolean())
    // Select a random value for each var
    vars1.map(v => v -> vals(scala.util.Random.nextInt(vals.size))).toMap
  Mut((0 to scala.util.Random.nextInt(2)).map(_ => randomAsgn()).toSet)

def randomFDD(vars: Seq[Var], vals: Seq[Val]) =
  def randomTest(vars1: Seq[Var]): FDD =
    if vars1.isEmpty then randomMut(vars, vals)
    else if scala.util.Random.nextBoolean() then randomTest(vars1.tail)
    else
      // Select a random subset of the vals
      val vals1 = vals.filter(_ => scala.util.Random.nextBoolean())
      Test(vars1.head, vals1.map(v => v -> randomTest(vars1.tail)).toMap, randomTest(vars1.tail))
  randomTest(vars.sorted)

def randFDD() = randomFDD(Seq("a", "b", "c"), Seq(1, 2, 3))

// A function to count the number of nodes in a FDD. Counts nodes only once if they are shared.
def countNodes(fdd: FDD): Int =
  val cache = scala.collection.mutable.Set.empty[FDD]
  def countNodes1(fdd: FDD): Int =
    if cache.contains(fdd) then 0
    else
      cache += fdd
      fdd match {
        case Mut(s) => s.size
        case Test(x, map, default) => 1 + map.values.map(countNodes1).sum + countNodes1(default)
      }
  countNodes1(fdd)

// A function to convert a fdd to graphviz format. Shares nodes.
// We have one node per Test, with "x = ?" in the node.
// We have edges from a Test to its branches and default case (labeled with "*").
// We display a Mut node as a square, and list each mutation on its own line. If empty, we display "drop".
def toGraphviz(fdd: FDD, idstart: Int = 0): String =
  def genColor(y: String): String = {
    if y == "*" then return "#000000"
    val x = y + "arsotienaorisetnarstadtt"
    val r = 0
    val g = (x.hashCode % 256).abs
    val b = (x.hashCode / 256 % 256).abs
    // Convert RGB to hex #RRGGBB
    f"#${r}%02x${g}%02x${b}%02x"
  }
  def genColor2(y: String) = {
    val x = y + "arsotienaorisetnarstadtt"
    val r = (x.hashCode % 256).abs
    val g = 200
    val b = (x.hashCode / 256 / 256 % 256).abs
    // Convert RGB to hex #RRGGBB
    f"#${r}%02x${g}%02x${b}%02x"
  }
  val sb = new StringBuilder
  var id = idstart
  // Adds a node to the graphviz output. Returns the node id.
  lazy val toGraphviz1: FDD => Int = memoize(fdd => {
    fdd match {
      case Mut(s) =>
        val id1 = id
        id += 1
        // We make Mut nodes square, and list each mutation on its own line
        val lines = s.map(m => "{" + m.map((v, x) => s"$v ← $x").mkString("; ") + "}").toSeq.sorted
        val out = if !lines.isEmpty then lines.mkString("\\n") else "drop"
        sb.append(s"""$id1 [shape=box, label="$out"]\n""")
        id1
      case Test(x, map, default) =>
        val id1 = id
        id += 1
        sb.append(s"""$id1 [label="$x = ?", style=filled, fillcolor="${genColor2(x)}"]\n""")
        map.foreach((v, fdd) => {
          sb.append(s"""$id1 -> ${toGraphviz1(fdd)} [label="$v", color="${genColor(v.toString)}", fontcolor="${genColor(v.toString)}"]\n""")
        })
        sb.append(s"""$id1 -> ${toGraphviz1(default)} [label="*", color="${genColor("*")}", fontcolor="${genColor("*")}"]\n""")
        id1
    }
  })
  toGraphviz1(fdd)
  sb.toString

// Saves a FDD to a file in graphviz format and converts it to pdf. Uses the the file ./fdd.pdf by default.
def saveGraphviz(fdds: Seq[FDD], file: String = "./fdd.pdf"): String = {
  // Only do this if the file wasn't touched in the last 2 seconds
  try {
    if System.currentTimeMillis - java.nio.file.Files.getLastModifiedTime(java.nio.file.Paths.get(file)).toMillis < 2000 then return "Waiting"
  } catch {
    case _: java.nio.file.NoSuchFileException => ()
  }
  import java.io._
  import sys.process._
  val gvcode = fdds.zipWithIndex
    .map { case (fdd, i) =>
      toGraphviz(fdd, i * 10000)
    }
    .mkString("\n")
  val pw = new PrintWriter(new File("fdd.gv"))
  pw.write("digraph G {\n")
  pw.write(gvcode)
  pw.write("}\n")
  pw.close
  s"dot -Tpdf fdd.gv -o $file".!
  "Updated"
}

// val z = randFDD()
val fdd = set("x" -> 1) * (test("y" -> 3) + test("z" -> 0)) * set("y" -> 3) + test("x" -> 5) + test("y" -> 6) + test("x" -> 4)
// val fdd2 = neg(fdd)
// saveGraphviz(fdd)
// val fdd2 = test("x" -> 1) * set("x" -> 2) + test("x" -> 2) * set("x" -> 3) + test("x" -> 3) * set("x" -> 2)
val fdd2 = fdd
val fdd3 = star(fdd2)
saveGraphviz(Seq(fdd, fdd3))

val x = randFDD()
// val y = randFDD()
// x * zero
// countNodes(x)
// countNodes(y)

// for _ <- 0 to 10 do
//   val x = randFDD()
//   val y = randFDD()
//   assert(x + y eq y + x)
//   assert(x + zero eq x)
//   assert(x * zero eq zero)

// set("x" -> 1) * set("x" -> 3)
// test("x" -> 1) * set("x" -> 2)
// set("x" -> 2) * test("x" -> 1)
// test("x" -> 1) * test("x" -> 1)

// val t = test("x" -> 3)
// val a = test("x" -> 3) + zero
// t eq a

// (zero + one) eq one
// one + one eq one
// t + t eq t
// t + test("y" -> 2)
// set("x" -> 2) + set("x" -> 3)

// val s = set("x" -> 3)
// s + s eq s

// val x = Test("x", Map(0 -> zero, 1 -> zero), one)
// val y = Test("x", Map(0 -> zero, 1 -> zero), one)
// x eq y
