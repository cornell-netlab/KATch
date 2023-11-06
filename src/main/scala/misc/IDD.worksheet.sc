import org.w3c.dom.Node
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

class IDD

case class LeafT() extends IDD
case class LeafF() extends IDD

case class Test(x: Var, left: IDD, right: IDD) extends IDD
object Test {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Test, Test]
  def apply(x: Var, left: IDD, right: IDD): IDD =
    if left == right then return left
    val v = new Test(x, left, right)
    cache.getOrElseUpdate(v, v)
}

case class Flip(x: Var, left: IDD, right: IDD) extends IDD
object Flip {
  private val cache = scala.collection.mutable.WeakHashMap.empty[Flip, Flip]
  def apply(x: Var, left: IDD, right: IDD): IDD =
    if left == zero then return right
    val v = new Flip(x, left, right)
    cache.getOrElseUpdate(v, v)
}

def toGraphviz(idd: IDD, prefix: String = "", showAll: Boolean = false): String =
  def genColor(y: String): String = {
    val x = y + "arsotienaorisetnarstadtt"
    val r = (x.hashCode % 256).abs
    val g = 200
    val b = (x.hashCode / 256 / 256 % 256).abs
    // Convert RGB to hex #RRGGBB
    f"#${r}%02x${g}%02x${b}%02x"
  }
  val sb = new StringBuilder
  var n = 0
  // Adds a node to the graphviz output. Returns the node id.
  lazy val toGraphviz1: IDD => String = memoize(idd => {
    n += 1
    val id = prefix + n.toString()
    val green = "#007F00"
    val red = "#AA0000"
    idd match {
      case LeafF() =>
        sb.append(s"""$id [label="0", style=filled, shape=point, width=0.2, fillcolor="red"]\n""")
      // sb.append(s"""$id [label= "", shape=none,height=.0,width=.0]\n""")
      case LeafT() =>
        sb.append(s"""$id [label="1", style=filled, shape=point, width=0.2, fillcolor="green"]\n""")
      case Flip(x, iddT, iddF) =>
        sb.append(s"""$id [label="!$x", style=filled, shape=diamond, fillcolor="${genColor(x)}"]\n""")
        if !(iddT eq zero) || showAll then sb.append(s"""$id -> ${toGraphviz1(iddT)} [label="Y", color="$green", fontcolor="$green"]\n""")
        if !(iddF eq zero) || showAll then sb.append(s"""$id -> ${toGraphviz1(iddF)} [label="N", color="$red", fontcolor="$red"]\n""")
      case Test(x, iddT, iddF) =>
        sb.append(s"""$id [label="$x?", style=filled, shape=circle, fillcolor="${genColor(x)}"]\n""")
        if !(iddT eq zero) || showAll then sb.append(s"""$id -> ${toGraphviz1(iddT)} [label="1", color="$green", fontcolor="$green"]\n""")
        if !(iddF eq zero) || showAll then sb.append(s"""$id -> ${toGraphviz1(iddF)} [label="0", color="$red", fontcolor="$red"]\n""")
    }
    id
  })
  val root = toGraphviz1(idd)
  sb.append(s"""source$prefix [label= "", shape=none,height=.0,width=.0]\n""")
  sb.append(s"""source$prefix -> $root\n""")
  if !(idd eq zero) || showAll then
    sb.append(s"""sink$prefix [label= "", shape=none,height=.0,width=.0]\n""")
    sb.append(s"""${toGraphviz1(one)} -> sink$prefix\n""")
  sb.toString

// Saves a IDD to a file in graphviz format and converts it to pdf. Uses the the file ./idd.pdf by default.
def saveGraphviz(idds: Seq[IDD], file: String = "./idd.pdf", showAll: Boolean = false): String = {
  // Only do this if the file wasn't touched in the last 2 seconds
  try {
    if System.currentTimeMillis - java.nio.file.Files.getLastModifiedTime(java.nio.file.Paths.get(file)).toMillis < 2000 then return "Waiting"
  } catch {
    case _: java.nio.file.NoSuchFileException => ()
  }
  import java.io._
  import sys.process._
  val gvcode = idds.zipWithIndex
    .map { case (idd, i) =>
      toGraphviz(idd, "idd" + i.toString() + "_", showAll)
    }
    .mkString("\n")
  val pw = new PrintWriter(new File("idd.gv"))
  pw.write("digraph G {\n")
  pw.write(gvcode)
  pw.write("}\n")
  pw.close
  s"dot -Tpdf idd.gv -o $file".!
  "Updated"
}

val zero: IDD = LeafF()
val one: IDD = LeafT()

def test(x: Var, v: Boolean) = if v then Test(x, one, zero) else Test(x, zero, one)
def flip(x: Var) = Flip(x, one, zero)
def set(x: Var, v: Boolean) =
  if v then Test(x, one, flip(x))
  else Test(x, flip(x), one)

// Sends all packets through both left and right, and joins the results.
lazy val plus: (IDD, IDD) => IDD = memoize2 { (left, right) =>
  (left, right) match {
    case (LeafF(), _) => right
    case (_, LeafF()) => left
    case (LeafT(), LeafT()) => right
    case (LeafT(), _) => plus(right, left)
    case (Test(xL, tL, fL), LeafT()) =>
      Test(xL, plus(tL, right), plus(fL, right))
    case (Flip(xL, tL, fL), LeafT()) =>
      Flip(xL, plus(tL, right), plus(fL, right))
    case (Test(xL, tL, fL), Test(xR, tR, fR)) =>
      if xL == xR then Test(xL, plus(tL, tR), plus(fL, fR))
      else if xL < xR then Test(xL, plus(tL, right), plus(fL, right))
      else plus(right, left)
    case (Test(xL, tL, fL), Flip(xR, tR, fR)) =>
      if xL <= xR then Test(xL, plus(tL, right), plus(fL, right))
      else Flip(xR, plus(left, tR), plus(left, fR))
    case (Flip(xL, tL, fL), Test(xR, tR, fR)) =>
      plus(right, left)
    case (Flip(xL, tL, fL), Flip(xR, tR, fR)) =>
      if xL == xR then Flip(xL, plus(tL, tR), plus(fL, fR))
      else if xL < xR then Flip(xL, plus(tL, right), plus(fL, right))
      else plus(right, left)
  }
}

// Sends all packets through both left and right, and intersects the results.
lazy val intersect: (IDD, IDD) => IDD = memoize2 { (left, right) =>
  (left, right) match {
    case (LeafF(), _) => zero
    case (_, LeafF()) => zero
    case (LeafT(), LeafT()) => right
    case (LeafT(), _) => intersect(right, left)
    case (Test(xL, tL, fL), LeafT()) =>
      Test(xL, intersect(tL, right), intersect(fL, right))
    case (Flip(xL, tL, fL), LeafT()) =>
      Flip(xL, intersect(tL, right), intersect(fL, right))
    case (Test(xL, tL, fL), Test(xR, tR, fR)) =>
      if xL == xR then Test(xL, intersect(tL, tR), intersect(fL, fR))
      else if xL < xR then Test(xL, intersect(tL, right), intersect(fL, right))
      else intersect(right, left)
    case (Test(xL, tL, fL), Flip(xR, tR, fR)) =>
      if xL <= xR then Test(xL, intersect(tL, right), intersect(fL, right))
      else Flip(xR, intersect(left, tR), intersect(left, fR))
    case (Flip(xL, tL, fL), Test(xR, tR, fR)) =>
      intersect(right, left)
    case (Flip(xL, tL, fL), Flip(xR, tR, fR)) =>
      if xL == xR then Flip(xL, intersect(tL, tR), intersect(fL, fR))
      else if xL < xR then Flip(xL, intersect(tL, right), intersect(fL, right))
      else intersect(right, left)
  }
}

// Sends all packets through both left and right, and takes the difference of the results.
lazy val difference: (IDD, IDD) => IDD = memoize2 { (left, right) =>
  (left, right) match {
    case (LeafF(), _) => zero
    case (_, LeafF()) => left
    case (LeafT(), LeafT()) => zero
    case (LeafT(), _) => difference(right, left)
    case (Test(xL, tL, fL), LeafT()) =>
      Test(xL, difference(tL, right), difference(fL, right))
    case (Flip(xL, tL, fL), LeafT()) =>
      Flip(xL, difference(tL, right), difference(fL, right))
    case (Test(xL, tL, fL), Test(xR, tR, fR)) =>
      if xL == xR then Test(xL, difference(tL, tR), difference(fL, fR))
      else if xL < xR then Test(xL, difference(tL, right), difference(fL, right))
      else difference(right, left)
    case (Test(xL, tL, fL), Flip(xR, tR, fR)) =>
      if xL <= xR then Test(xL, difference(tL, right), difference(fL, right))
      else Flip(xR, difference(left, tR), difference(left, fR))
    case (Flip(xL, tL, fL), Test(xR, tR, fR)) =>
      difference(right, left)
    case (Flip(xL, tL, fL), Flip(xR, tR, fR)) =>
      if xL == xR then Flip(xL, difference(tL, tR), difference(fL, fR))
      else if xL < xR then Flip(xL, difference(tL, right), difference(fL, right))
      else difference(right, left)
  }
}

def mkTest(x: Var, v: Boolean, t: IDD, f: IDD): IDD = if v then Test(x, t, zero) else Test(x, zero, f)

// Specializes an IDD to a given value of an input variable (precompose with test).
lazy val spec: (Var, Boolean, IDD) => IDD = memoize3 { (x, v, idd) =>
  idd match {
    case LeafF() => zero
    case LeafT() => test(x, v)
    case Test(y, t, f) =>
      if x == y then mkTest(y, v, t, f)
      else if x < y then mkTest(x, v, idd, idd)
      else Test(y, spec(x, v, t), spec(x, v, f))
    case Flip(y, t, f) =>
      if x <= y then mkTest(x, v, idd, idd)
      else Flip(y, spec(x, v, t), spec(x, v, f))
  }
}

// Flips the value of an input variable (precompose with flip).
lazy val flip: (Var, IDD) => IDD = memoize2 { (x, idd) =>
  idd match {
    case LeafF() => zero
    case LeafT() => Flip(x, one, zero)
    case Test(y, t, f) =>
      if x == y then Test(y, flip(x, f), flip(x, t))
      else if x < y then Flip(x, idd, zero)
      else Test(y, flip(x, t), flip(x, f))
    case Flip(y, t, f) =>
      if x == y then Flip(y, f, t)
      else if x < y then Flip(x, idd, zero)
      else Flip(y, flip(x, t), flip(x, f))
  }
}

// Sends all packets through left, and then sends the resulting packets through right.
lazy val seq: (IDD, IDD) => IDD = memoize2 { (left, right) =>
  (left, right) match {
    case (LeafF(), _) => zero
    case (_, LeafF()) => zero
    case (LeafT(), _) => right
    case (_, LeafT()) => left
    case (Test(x, t, f), _) =>
      spec(x, true, seq(t, right)) + spec(x, false, seq(f, right))
    case (Flip(x, t, f), _) =>
      seq(t, flip(x, right)) + seq(f, right)
  }
}

def star(idd: IDD): IDD =
  def fix(x: IDD): IDD =
    val y = plus(one, seq(x, idd))
    if x eq y then y else fix(y)
  fix(zero)

// Syntactic sugar for plus and seq.
implicit class IDDOps(left: IDD) {
  def +(right: IDD): IDD = plus(left, right)
  def *(right: IDD): IDD = seq(left, right)
}

type Packet = Map[Var, Boolean]

// Run a packet through an IDD.
lazy val run: (IDD, Packet) => Set[Packet] = memoize2 { (idd, packet) =>
  idd match {
    case LeafF() => Set.empty
    case LeafT() => Set(packet)
    case Test(x, t, f) =>
      if packet(x) then run(t, packet) else run(f, packet)
    case Flip(x, t, f) =>
      run(t, packet.updated(x, !packet(x))) ++ run(f, packet)
  }
}

// δ : NK → Map[NK, IDD]
// ε : NK → IDD

// δ(e*) := ε(e)* ⋅ δ(e) ⋅ e*
// δ(e1 ⋅ e2) := δ(e1) ⋅ e2 + ε(e1) ⋅ δ(e2)

// a -> b -> 0
// c -> 0

// val x = set("x", false) + set("x", true)

val a = "a"
val x = "x"
val y = "y"
val z = "z"
// val idd = flip(x) * flip(x)
// val idd = flip(x) * test(x, true)
// val idd = test(x, false) * flip(x)
// val idd = (flip(x) + one) * test(x, true)
// val idd0 = (flip(x) + one) * test(x, true)
val idd0 = flip(x) + test(y, true)
val idd1 = flip(y) + test(x, true)
// val idd = idd0 * idd1
// val idd = set(x, true) + test(x,false) * set(x, false)
val idd2 = idd0 * idd1 + test(z, true) * set(z, false)
// val idd = intersect(idd2, test(x, false) * test(y, true) * test(z, true))
val idd = idd2

saveGraphviz(Seq(idd), showAll = true)

class NK
case object NDup extends NK
case class NKTest(x: Var, v: Boolean) extends NK
case class NKFlip(x: Var) extends NK
case class NKSeq(es: List[NK]) extends NK
case class NKSum(es: Set[NK]) extends NK
case class NKStar(e: NK) extends NK

val nZero = new NKSum(Set())
val nOne = new NKSeq(List())

object nTest {
  private val cache = scala.collection.mutable.WeakHashMap.empty[NKTest, NKTest]
  def apply(x: Var, v: Boolean): NK =
    val w = new NKTest(x, v)
    cache.getOrElseUpdate(w, w)
}

object nFlip {
  private val cache = scala.collection.mutable.WeakHashMap.empty[NKFlip, NKFlip]
  def apply(x: Var): NK =
    val w = new NKFlip(x)
    cache.getOrElseUpdate(w, w)
}

object nSeq {
  private val cache = scala.collection.mutable.WeakHashMap.empty[NKSeq, NKSeq]
  def apply(es: List[NK]): NK =
    val es2 = es.flatMap { case NKSeq(es) => es; case e => List(e) }
    if es2.length == 1 then return es2.head
    val w = new NKSeq(es2)
    cache.getOrElseUpdate(w, w)
}

object nSum {
  private val cache = scala.collection.mutable.WeakHashMap.empty[NKSum, NKSum]
  def apply(es: Set[NK]): NK =
    val es2 = es.flatMap { case NKSum(es) => es; case e => Set(e) }
    if es2.size == 1 then return es2.head
    val w = new NKSum(es2)
    cache.getOrElseUpdate(w, w)
}

object nStar {
  private val cache = scala.collection.mutable.WeakHashMap.empty[NKStar, NKStar]
  def apply(e: NK): NK =
    e match {
      case NKSum(es) if es.isEmpty => nOne
      case NKSeq(es) if es.isEmpty => nOne
      case NKStar(e) => e
      case _ =>
        val w = new NKStar(e)
        cache.getOrElseUpdate(w, w)
    }
}

// The IDD for packets that don't go through a dup in the expression.
lazy val ε: NK => IDD = memoize { e =>
  e match {
    case NDup => zero
    case NKTest(x, v) => test(x, v)
    case NKFlip(x) => flip(x)
    case NKSeq(es) => es.foldLeft(one) { case (acc, e) => acc * ε(e) }
    case NKSum(es) => es.foldLeft(zero) { case (acc, e) => acc + ε(e) }
    case NKStar(e) => star(ε(e))
  }
}

// Applies a function to the keys of a map.
// We have to restore the invariant that the idds are disjoint, if the function is not injective.
def mapKeys(m: Map[NK, IDD], f: NK => NK) =
  var m2: Map[NK, IDD] = Map.empty
  for (e, idd) <- m do
    val e2 = f(e)
    m2 = m2.updated(e2, m2.getOrElse(e2, zero) + idd)
  m2

// Adds an entry to a map.
// We have to restore the invariant that the idds are disjoint.
// To do so, we have to intersect the idd with the idds that are already in the map.
// If the intersection is non-zero, we add entries to the map for the sum of the expressions.
def addMapEntry(m: Map[NK, IDD], e: NK, idd: IDD): Map[NK, IDD] =
  if idd eq zero then return m
  if m.keySet.contains(e) then return m.updated(e, m(e) + idd)
  var rem = idd
  var m2 = Map.empty[NK, IDD]
  def addEntry(e: NK, idd: IDD) =
    if !(idd eq zero) then m2 = m2.updated(e, m2.getOrElse(e, zero) + idd)
  for (e2, idd2) <- m do
    val inter = intersect(rem, idd2)
    addEntry(nSum(Set(e, e2)), inter)
    addEntry(e2, difference(idd2, inter))
    rem = difference(rem, inter)
  addEntry(e, rem)
  m2

// The two maps represent a NetKAT expression, namely
//   sum(idd * e for e -> idd in m1)
//   sum(idd * e for e -> idd in m2)
// These expressions are deterministic in the sense that the idds are disjoint.
// We want to compute the sum of these two expressions,
// and we want to maintain the invariant that the idds are disjoint.
def mapAdd(m1: Map[NK, IDD], m2: Map[NK, IDD]) =
  m1.foldLeft(m2) { case (m, (e, idd)) => addMapEntry(m, e, idd) } // this can be optimized if we can assume that m2 is in normal form (but then we need to fix up the next function)
  // We might also be able to optimise based on how the NetKAT expressions sum

// Restores the invariant that the idds are disjoint. Also removes zero idds.
def restoreInvariant(m: Map[NK, IDD]): Map[NK, IDD] =
  mapAdd(Map.empty, m)
  // an alternative approach would be to recurse on the structure of the idds
  // if we do it idd by idd, maybe it would be equivalent to computing the following function:
  // def cases(idd1: IDD, idd2: IDD): (IDD,IDD,IDD) =
  //   (difference(idd1, idd2), intersect(idd1, idd2), difference(idd2, idd1))

// Applies a function to the values of a map.
// We have to restore the invariant that the idds are disjoint.
def mapVals(m: Map[NK, IDD], f: IDD => IDD) =
  restoreInvariant(m.map((e, idd) => (e, f(idd))))

// The transition function of the automaton.
lazy val δ: NK => Map[NK, IDD] = memoize { e =>
  e match {
    case NDup => Map(nOne -> one)
    case NKTest(_, _) | NKFlip(_) => Map()
    case NKSeq(es) =>
      es match {
        case Nil => Map.empty
        case e :: es2 =>
          val δ1 = mapKeys(δ(e), e2 => nSeq(e2 :: es2))
          val δ2 = mapVals(δ(nSeq(es2)), idd => seq(ε(e), idd))
          mapAdd(δ1, δ2)
      }
    case NKSum(es) => es.foldLeft(Map.empty) { case (acc, e) => mapAdd(acc, δ(e)) }
    case NKStar(e2) => mapVals(mapKeys(δ(e2), e3 => nSeq(List(e3, e))), idd => seq(star(ε(e2)), idd))
  }
}

// FIXME: the code above is incorrect for at least the NKSum case
// The problem is that we need to uphold the invariant that the idds in the map are disjoint and sum to one.
// Maybe fixing mapAdd will fix this, but we have to check the other cases too.

// TODO: make visualiser
// TODO: test on examples
// TODO: compute product automaton
// TODO: bisimulation
